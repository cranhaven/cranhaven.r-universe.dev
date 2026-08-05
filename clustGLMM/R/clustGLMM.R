clustGLMM <- 
function(
  formula,   # list of formulas for each of the variables
  id,        # column name of IDs declaring which observations come from the same unit
  family,    # named vector of family types 
  data,      # a data.frame object containing outcomes, regressors and id
  G,         # total number of latent Groups=components (can be different for all chains)
  varying,   # which parameters should be class-specific
             # order: prec_num, c_ord, InvSigma, InvQ, naY
  save,      # what parameters should be saved, in order:
             # "beta_num", "beta_num_grp",
             # "prec_num", "sd_num", "var_num",
             # "beta_poi", "beta_poi_grp",
             # "beta_bin", "beta_bin_grp",
             # "beta_ord", "beta_ord_grp",
             # "a_ord", "c_ord", "pi_ord",
             # "beta_cat", "beta_cat_grp",
             # "InvSigma", "Sigma", "sdSigma", "corSigma", "detInvSigma",
             # "InvQ", "Q", "detInvQ",
             # "b",
             # "w", "ng",
             # "loglik", "pUig", "U",
             # "Gplus", "e0"
  param,     # list of hyperparameters for prior distributions
  tuning,    # list of tuning parameters
  inits,     # list of initial values from which to start
             # if missing or NULL --> create your own
  iter = 1000, # length of sampled chain
  nchains = 1, # number of chains
  standardize = TRUE, # should the numeric outcomes and regressors be standardized?
  howsave = c("data.frame", "list", "cmcmc")  # how should be the output MCMC chains returned?
             # data.frame - in a data.frame with special column names
             # list - in lists of lists of parameters...
){
  # TODO make these function parameters
  howsave <- match.arg(howsave)
    
  # require("MASS")
  # require("nnet")
  
  ### Missing values from input
  if(missing(G)){
    G <- rep(2, nchains)
  }else{
    if(length(G) == 1){
      G <- rep(G, nchains)
    }else{
      if(length(G) != nchains){
        stop("You have given several G values but a different number of chains. 
             Declare precisely with how many components G should each chain be estimated.")
      }
    }
  }
  if (missing(varying)){
    varying <- default_varying()
  }
  # correction of possible mistakes 
  # - if deeper variables is class-specific then the dependent one must be as well
  # prior for InvSigma depends on Invq
  if(varying["InvQ"]){varying["InvSigma"] <- TRUE}
  
  if(missing(save)){
    save <- default_save()
  }
  
  important <- c("beta_num_fix", "beta_num",
                 "prec_num",
                 "beta_poi_fix", "beta_poi",
                 "beta_bin_fix", "beta_bin",
                 "beta_ord_fix", "beta_ord",
                 "c_ord",
                 "beta_cat_fix", "beta_cat",
                 "InvSigma", 
                 "w")
  
  if(!all(save[important])){
    warning("Some of the parameters: prec_num, betas, c_ord, InvSigma, w will not be saved.
Therefore, you will not be able to calculate unconditional classification probabilities of new subjects.
Change the save settings if you want to calculate these probabilities afterwards.")
  }
  
  if(missing(param)){
    param <- default_param()
  } # end of if missing(param)
  
  if(missing(tuning)){
    tuning <- default_tuning()
  }
  
  ### Most important auxiliary variables
  mcmc <- list()
  N <- dim(data)[1]
  unique_ids <- unique(data[,id])
  n <- length(unique_ids)
  numbered_unique_ids <- c(1:n)
  names(numbered_unique_ids) <- unique_ids
  nsubj <- table(data[,id])
  
  ### Sorting family types
  Nums <- names(family)[is.element(family, c("num", "gaussian"))]
  Pois <- names(family)[is.element(family, c("poi", "poisson"))]
  Bins <- names(family)[is.element(family, c("bin", "bernoulli"))]
  Ords <- names(family)[is.element(family, c("ord", "cumulative"))]
  Cats <- names(family)[is.element(family, c("cat", "categorical"))]
  Ys <- c(Nums, Pois, Bins, Ords, Cats)
  nY <- sapply(list(Nums, Pois, Bins, Ords, Cats), length)
  names(nY) <- c("Nums", "Pois", "Bins", "Ords", "Cats")
  
  # sorted_family <- c(family[Nums], family[Pois], family[Bins], family[Ords], family[Cats])
  sorted_family <- c(rep("num", length(Nums)),
                     rep("poi", length(Pois)),
                     rep("bin", length(Bins)),
                     rep("ord", length(Ords)),
                     rep("cat", length(Cats)))
  names(sorted_family) <- Ys
  if(length(sorted_family) < length(family)){
    not_matched_outcomes <- setdiff(names(family), names(sorted_family))
    stop(paste0("Families of outcomes: ", paste(not_matched_outcomes, collapse = ", "), 
                "... do not match any allowed family type. 
                See help page for the list of accepted family types."))
  }
  if(!identical(names(family), names(sorted_family))){
    warning("family argument has been reordered by type: num, poi, bin, ord, cat. 
            This is also the ordering used for the random effects.")
  }
  
  ### Checking family types + transformations
  ydata <- data
  centers <- scales <- list()
  # Numeric
  if(nY["Nums"] > 0){
    for(y in Nums){
      # subtract the offset immediately
      if(is.null(formula[[y]]$offset) || (formula[[y]]$offset == "")){
        yval <- data[,y]
      }else{
        if(is.element(formula[[y]]$offset, colnames(data))){
          yval <- data[,y] - data[,formula[[y]]$offset]
        }else{
          stop(paste0("Data.frame data does not contain an offset ", formula[[y]]$offset, "."))
        }
      }
      if(!is.numeric(yval)){
        stop(paste0("Outcome ", y, " is not numeric!"))
      }
      if(any(is.infinite(yval))){
        stop(paste0("There are infinite values for outcome ", y, "!"))
      }
      if(standardize){
        centers[[y]] <- mean(yval, na.rm = TRUE)
        scales[[y]] <- sd(yval, na.rm = TRUE)
        ydata[,y] <- scale(yval, center = centers[[y]], scale = scales[[y]]) # numeric values
      }else{
        centers[[y]] <- 0.0
        scales[[y]] <- 1.0
        ydata[,y] <- yval # numeric values
      }
    }
  }
  centers <- unlist(centers)
  scales <- unlist(scales)
  
  
  # Poisson
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){abs(x - as.integer(x)) < tol}
  if(nY["Pois"] > 0){
    for(y in Pois){
      yval <- data[,y]
      if(any(is.infinite(yval))){
        stop(paste0("There are infinite values for outcome ", y, "!"))
      }
      if(!all(is.wholenumber(yval), na.rm = TRUE)){
        stop(paste0("Outcome ", y, " is not integer-valued!"))
      }
      if(!all(yval >= 0, na.rm = TRUE)){
        stop(paste0("Outcome ", y, " has some negative values!"))
      }
      ydata[,y] <- yval # integer values
    }
  }
  
  # Binary
  if(nY["Bins"] > 0){
    for(y in Bins){
      fy <- factor(data[,y])
      if(nlevels(fy) != 2){
        stop(paste0("Binary outcome ", y, " does not have 2 levels."))
      }
      ydata[,y] <- as.numeric(fy)-1 # 0, 1 values
    }
  }
  
  # Ordinal
  if(nY["Ords"] > 0){
    Kord <- numeric(length(Ords))
    names(Kord) <- Ords
    for(y in Ords){
      fy <- data[,y]
      if(!is.ordered(fy)){
        fy <- factor(fy, ordered = TRUE)
        warning(paste0("Converting ordinal outcome ", y, " to ordered factor with levels: ",
                       paste(levels(fy), collapse = " < ")))
      }
      if(nlevels(fy) <= 2){
        stop(paste0("Ordinal outcome ", y, " has less than 3 levels."))
      }
      ydata[,y] <- as.numeric(fy)-1 # 0 < 1 < ... < Kord[y]
      Kord[y] <- nlevels(fy)-1
    }
  }else{
    Kord <- NULL
  }
  
  # Categorical
  if(nY["Cats"] > 0){
    Kcat <- numeric(length(Cats))
    names(Kcat) <- Cats
    for(y in Cats){
      fy <- data[,y]
      if(!is.factor(fy)){
        fy <- factor(fy)
        warning(paste0("Converting outcome  ", y, " into categorical factor with levels: ",
                       paste(levels(fy), collapse = ", ")))
      }
      if(nlevels(fy) <= 2){
        stop(paste0("Categorical outcome ", y, " has less than 3 levels."))
      }
      # First category is assumed to be the first one here
      # Later for C function changed to the last one
      ydata[,y] <- as.numeric(fy)-1 # 0, 1, ..., Kcat[y]
      Kcat[y] <- nlevels(fy)-1
      if(!is.null(formula[[y]]$offset) && (formula[[y]]$offset != "")){
        warning(paste0("Offsetting categorical variable is prohibited. Continuing without the offset for outcome ", y, "."))
        formula[[y]]$offset <- ""
      }
    }
  }else{
    Kcat <- NULL
  }
  
  if(length(unique(Ys)) != length(Ys)){
    stop("Some outcome names are being repeated.")
  }
  
 
  nfix <- ngrp <- nran <- noff <- numeric(sum(nY))
  names(nfix) <- names(ngrp) <- names(nran) <- names(noff) <- Ys
  fixnames <- grpnames <- rannames <- offnames <- numeric()
  lfixnames <- lgrpnames <- lrannames <- loffnames <- list()
  # Creating model matrix containing all needed columns for C
  finX <- data.frame(id = data[,id])
  Xcolnames <- numeric()
  fcols <- gcols <- rcols <- ocols <- list()
  for(y in Ys){
    if(is.null(formula[[y]]$offset)){
      formula[[y]]$offset <- ""
    }
    fauxX <- model.matrix(formula[[y]]$fixed, data)
    gauxX <- model.matrix(formula[[y]]$group, data)
    rauxX <- model.matrix(formula[[y]]$random, data)
    if(is.element(y, Ords)){
      fcols[[y]] <- setdiff(colnames(fauxX), c("(Intercept)"))
      gcols[[y]] <- setdiff(colnames(gauxX), c("(Intercept)"))
    }else{
      fcols[[y]] <- colnames(fauxX)
      gcols[[y]] <- colnames(gauxX)
    }
    # Regressors are preferably group-specific
    fcols[[y]] <- setdiff(fcols[[y]], gcols[[y]])
    rcols[[y]] <- colnames(rauxX)
    
    # intercept inclusion checks
    if(standardize){
      if(is.element(y, Ords)){
        if((length(gcols[[y]]) > 0) & (!varying["c_ord"])){
          warning("If standardize = TRUE, when group is nonempty for ordinal outcome,
                  the ordered intercepts c_ord have to be group-specific.
                  Otherwise, it would be impossible to rescale parameters back. 
                  Group-specificity of c_ord changed to TRUE.")
          varying["c_ord"] <- TRUE
        }
      }else{
        if(!is.element("(Intercept)", union(fcols[[y]], gcols[[y]]))){
          stop("standardize = TRUE, but column (Intercept) not included in either fixed or group formula. 
               Then, it would not be possible to transform betas back to original scale.")
        }
        if(length(gcols[[y]]) > 0){
          if(is.element("(Intercept)", fcols[[y]])){
            # (Intercept) in fixed and non-empty group-specificity
            # --> has to be group-specific as well
            warning("If standardize = TRUE, fixed contains (Intercept) and group is non-empty.
                    (Intercept) is moved to group-specific effects in order to be able to scale parameters back.")
            fcols[[y]] <- setdiff(fcols[[y]], "(Intercept)")
            gcols[[y]] <- union("(Intercept)", gcols[[y]])
            gauxX <- transform(gauxX, "(Intercept)"= 1.0)
          }
        }
      }
      if((length(rcols[[y]]) > 0) & (!is.element("(Intercept)", rcols[[y]]))){
        stop("standardize = TRUE, but column (Intercept) not included in random formula, which is non-empty. 
             Then, it would be difficult to transform random effects back to original scale.")
      }
    }
    
    notf <- setdiff(fcols[[y]], Xcolnames)
    Xcolnames <- c(Xcolnames, notf)
    addX <- data.frame(fauxX[,notf])
    colnames(addX) <- notf
    finX <- cbind(finX, addX)
    
    notg <- setdiff(gcols[[y]], Xcolnames)
    Xcolnames <- c(Xcolnames, notg)
    addX <- data.frame(gauxX[,notg])
    colnames(addX) <- notg
    finX <- cbind(finX, addX)
    
    notr <- setdiff(rcols[[y]], Xcolnames)
    Xcolnames <- c(Xcolnames, notr)
    addX <- data.frame(rauxX[,notr])
    colnames(addX) <- notr
    finX <- cbind(finX, addX)
    
    ocols[[y]] <- formula[[y]]$offset
    loffnames[[y]] <- as.character(c())
    noff[y] <- ifelse((formula[[y]]$offset == "") || (sorted_family[y] == "num"), 0, 1)
    if(noff[y] > 0){
      #if numeric --> will not be added to loffnames
      #but ocols and formula still remember that there has been some
      loffnames[[y]] <- ocols[[y]]
      noto <- setdiff(ocols[[y]], Xcolnames)
      Xcolnames <- c(Xcolnames, noto)
      addX <- data.frame(data[,noto])
      colnames(addX) <- noto
      finX <- cbind(finX, addX)
    }
    
    nfix[y] <- length(fcols[[y]])
    ngrp[y] <- length(gcols[[y]])
    nran[y] <- length(rcols[[y]])
    lfixnames[[y]] <- fcols[[y]]
    lgrpnames[[y]] <- gcols[[y]]
    lrannames[[y]] <- rcols[[y]]
    
    fixnames <- c(fixnames, lfixnames[[y]])
    grpnames <- c(grpnames, lgrpnames[[y]])
    rannames <- c(rannames, lrannames[[y]])
    offnames <- c(offnames, loffnames[[y]])
  }
  if(anyNA(finX)){
    stop("There are NA values among regressors, which is not supported.")
  }
  # Scale regressors in finX
  if(length(intersect(offnames, c(grpnames, fixnames, rannames))) > 0){
    stop("Offset variables are not allowed be used as predictor variables, please, rename.")
  }
  std_cols <- setdiff(colnames(finX), c("id", "(Intercept)", offnames))
  if(standardize){
    finX_scaled <- scale(finX[,std_cols,drop=FALSE], center = TRUE, scale = TRUE)
    finX[,std_cols] <- ydata[,std_cols] <- finX_scaled
    centers <- c(centers, attr(finX_scaled, "scaled:center"))
    scales <- c(scales, attr(finX_scaled, "scaled:scale"))
  }else{
    ydata[,std_cols] <- finX[,std_cols,drop=FALSE]
    addc <- rep(0.0, length(std_cols))
    adds <- rep(1.0, length(std_cols))
    names(addc) <- names(adds) <- std_cols
    centers <- c(centers, addc)
    scales <- c(scales, adds)
  }
  
  totnran <- sum(nran)+ifelse(tuning$integer$kspec_bi_cat, sum(nran[Cats]*Kcat)-sum(nran[Cats]), 0)
  # totnfix <- sum(nfix)-sum(nfix[Cats])+sum(nfix[Cats]*Kcat)
  # totngrp <- G * (sum(ngrp)-sum(ngrp[Cats])+sum(ngrp[Cats]*Kcat))
  
  if(totnran == 0){
    message("There are no random effects specific for each subject.
            InvSigma, InvQ, b and other related parameters will not exist.")
    save[c("InvSigma", "Sigma", "sdSigma", "corSigma", "detInvSigma",
           "InvQ", "Q", "detInvQ",
           "b")] <- FALSE
    if((N > n) & (sum(nY) > 1)){
      warning("Some subjects have more than one observation and no random effects are considered. 
Therefore, the given outcomes are considered independent. ")
    }
  }
  
  # if(N != dim(X)[1]){stop("Y and X have different number of rows.")}
  
  
  # update param depending on the total dimension of random effects
  param$InvSigma_df <- param$InvSigma_df + totnran
  param$InvQ_df <- param$InvQ_df + totnran
  param$InvV <- diag(param$InvV, totnran)
  
  
  ### Initial values computation
  # Initial values are for scaled parameters!!!
  initsgiven = 1
  if(missing(inits)){
    message("Inits are missing, calculating...\n")
    initsgiven = 0
    inits <- list()
    for(ch in 1:nchains){
      inits[[ch]] <- list()
      
      # parameters connected to classification
      inits[[ch]]$w <- rep(1/G[ch], G[ch])
      inits[[ch]]$U <- sample(1:G[ch], n, replace = TRUE, prob = inits[[ch]]$w)
      inits[[ch]]$e0 <- param$e0_shp / param$e0_rte
      #inits[[ch]]$U <- as.numeric(as.character(data$g[data$j==1]))
      Ng <- selection <- list()
      for(g in 1:G[ch]){
        Ng[[g]] <- unique_ids[inits[[ch]]$U == g]
        selection[[g]] <- is.element(data[,id], Ng[[g]])
      }
      inits[[ch]]$pUig <- matrix(rep(inits[[ch]]$w, n), ncol = G[ch], nrow = n, byrow = TRUE)
      
      # parameters connected to numerical variables --> use linear regression
      inits[[ch]]$beta_num <- inits[[ch]]$beta_num_fix <- inits[[ch]]$prec_num <- list()
      for(y in Nums){
        Xvars <- c(fcols[[y]], gcols[[y]])
        # Using all data
        nonna <- (!is.na(ydata[,y]))
        pomY <- ydata[nonna,y] # - apply(finX[nonna, loffnames[[y]], drop = FALSE], 1, sum)
        boldX <- as.matrix(finX[nonna, Xvars, drop=FALSE])
        fit0 <- lm.fit(boldX, pomY)
        coefs <- coef(fit0)
        MSe <- sum(residuals(fit0)^2) / fit0$df.residual
        init_prec_num <- 1/MSe
        inits[[ch]]$beta_num_fix[[y]] <- coefs[fcols[[y]]]
        
        if(varying["prec_num"]){
          inits[[ch]]$prec_num[[y]] <- list()
        }else{
          inits[[ch]]$prec_num[[y]] <- init_prec_num
        }
        
        inits[[ch]]$beta_num[[y]] <- list()
        for(g in 1:G[ch]){
          inds <- selection[[g]] & !is.na(ydata[,y])
          pomY <- ydata[inds,y] # - apply(finX[inds, loffnames[[y]], drop = FALSE], 1, sum)
          boldX <- as.matrix(finX[inds, Xvars, drop=FALSE])
          fit <- try(lm.fit(boldX, pomY))
          if (is(fit, "try-error") || anyNA(coef(fit))) {
              fit <- fit0
          }
          coefs <- coef(fit)
          MSe <- sum(residuals(fit)^2) / fit$df.residual
          if(varying["prec_num"]){
            inits[[ch]]$prec_num[[y]][[g]] <- 1/MSe
          }
          inits[[ch]]$beta_num[[y]][[g]] <- coefs[gcols[[y]]]
        } # end of for g in 1:G[ch]
      } # end of for y in Nums
      
      # parameters connected to Poisson variables --> use log-linear model
      add_intercept <- function(formula) {
          formula_str <- paste0(deparse(formula), collapse = "")
          if (!grepl("(0|- 1)", formula_str)) {
              formula <- as.formula(paste0(formula_str, "+ 1"),
                                    environment(formula))
          }
          return(formula)
      }
      inits[[ch]]$beta_poi <- inits[[ch]]$beta_poi_fix <- list()
      for(y in Pois){
        aux_form <- paste0(y, " ~ ",
                           formula[[y]]$fixed[2],
                           " + ",
                           add_intercept(formula[[y]]$group)[2]
                           )
        if(noff[y] > 0){
          aux_form <- paste0(aux_form, "+ offset(",
                             formula[[y]]$offset,
                             ")")
        }
        auxglm0 <- glm(aux_form, data = ydata, family = poisson(link = "log"))
        coefs <- auxglm0$coefficients
        beta_fix <- coefs[fcols[[y]]]
        # if(standardize){
        #   # scale to the transformed scale
        #   notintercept <- setdiff(fcols[[y]], "(Intercept)")
        #   beta_fix[notintercept] <- beta_fix[notintercept] * scales[notintercept]
        #   if(is.element("(Intercept)", fcols[[y]])){
        #     # then gcols has to be empty
        #     beta_fix["(Intercept)"] <- beta_fix["(Intercept)"] + sum(beta_fix[notintercept]*centers[notintercept])
        #   }
        # }
        inits[[ch]]$beta_poi_fix[[y]] <- beta_fix
        
        inits[[ch]]$beta_poi[[y]] <- list()
        for(g in 1:G[ch]){
          auxdata <- ydata[selection[[g]],,drop=FALSE]
          auxglm <- tryCatch(glm(aux_form, data = auxdata,
                                 family = poisson(link = "log")),
                             error=function(e) e, warning=function(w) w)
          # if(is(auxglm,"warning") || is(auxglm,"error")){
          if(is(auxglm,"error")){
            auxglm <- auxglm0
          }else{
            if(!auxglm$converged){
              auxglm <- auxglm0
            }
          }
          coefs <- auxglm$coefficients
          if(sum(is.na(coefs))>0){
            coefs <- auxglm0$coefficients
          }
          beta <- coefs[gcols[[y]]]
          # if(standardize){
          #   # scale to the transformed scale
          #   notintercept <- setdiff(gcols[[y]], "(Intercept)")
          #   beta[notintercept] <- beta[notintercept] * scales[notintercept]
          #   if(is.element("(Intercept)", gcols[[y]])){
          #     beta["(Intercept)"] <- beta["(Intercept)"] + sum(beta_fix[fcols[[y]]]*centers[fcols[[y]]]) + sum(beta[notintercept]*centers[notintercept])
          #   }
          # }
          inits[[ch]]$beta_poi[[y]][[g]] <- beta
        }
      } # end for y in Pois
      
      # parameters connected to binary variables --> use logistic regression
      inits[[ch]]$beta_bin <- inits[[ch]]$beta_bin_fix <- list()
      for(y in Bins){
        aux_form <- paste0(y, " ~ ",
                           formula[[y]]$fixed[2],
                           " + ", 
                           add_intercept(formula[[y]]$group)[2]
        )
        if(noff[y] > 0){
          aux_form <- paste0(aux_form, "+ offset(",
                             formula[[y]]$offset,
                             ")")
        }
        auxglm0 <- glm(aux_form, data = ydata, family = binomial(link = "logit"))
        coefs <- auxglm0$coefficients
        beta_fix <- coefs[fcols[[y]]]
        # if(standardize){
        #   # scale to the transformed scale
        #   notintercept <- setdiff(fcols[[y]], "(Intercept)")
        #   beta_fix[notintercept] <- beta_fix[notintercept] * scales[notintercept]
        #   if(is.element("(Intercept)", fcols[[y]])){
        #     # then gcols has to be empty
        #     beta_fix["(Intercept)"] <- beta_fix["(Intercept)"] + sum(beta_fix[notintercept]*centers[notintercept])
        #   }
        # }
        inits[[ch]]$beta_bin_fix[[y]] <- beta_fix
        
        inits[[ch]]$beta_bin[[y]] <- list()
        for(g in 1:G[ch]){
          auxdata <- ydata[selection[[g]],,drop=FALSE]
          auxglm <- tryCatch(glm(aux_form, data = auxdata,
                                 family = binomial(link = "logit")),
                             error=function(e) e, warning=function(w) w)
          # if(is(auxglm,"warning") || is(auxglm,"error")){
          if(is(auxglm,"error")){
            auxglm <- auxglm0
          }else{
            if((!auxglm$converged) || (sum(is.na(auxglm$coefficients[gcols[[y]]]))>0)){
              auxglm <- auxglm0
            }
          }
          coefs <- auxglm$coefficients
          if(sum(is.na(coefs))>0){
            coefs <- auxglm0$coefficients
          }
          beta <- coefs[gcols[[y]]]
          # if(standardize){
          #   # scale to the transformed scale
          #   notintercept <- setdiff(gcols[[y]], "(Intercept)")
          #   beta[notintercept] <- beta[notintercept] * scales[notintercept]
          #   if(is.element("(Intercept)", gcols[[y]])){
          #     beta["(Intercept)"] <- beta["(Intercept)"] + sum(beta_fix[fcols[[y]]]*centers[fcols[[y]]]) + sum(beta[notintercept]*centers[notintercept])
          #   }
          # }
          inits[[ch]]$beta_bin[[y]][[g]] <- beta
        }
      } # end for y in Bins
      
      # parameters connected to ordinal variables --> use ordered logistic regression
      # library("MASS")
      inits[[ch]]$beta_ord <- inits[[ch]]$beta_ord_fix <- inits[[ch]]$a_ord <- inits[[ch]]$c_ord <- inits[[ch]]$pi_ord <-list()
      for(y in Ords){
        aux_form <- paste0(y, " ~ .")
        if(noff[y] > 0){
          aux_form <- paste0(aux_form, "+ offset(",formula[[y]]$offset,")")
          auxdata0 <- finX[,union(fcols[[y]], union(gcols[[y]], ocols[[y]])),drop=FALSE]
        }else{
          auxdata0 <- finX[,union(fcols[[y]], gcols[[y]]),drop=FALSE]
        }
        # possibly standardized regressors
        auxdata0[,y] <- factor(ydata[,y])
        auxpolr0 <- polr(aux_form, data = auxdata0)
        if(length(fcols[[y]]) > 0){
          fc <- sapply(fcols[[y]], function(x){
            xdash <- paste0("`", x, "`")
            if(is.element(xdash, names(auxpolr0$coefficients))){
              return(xdash)
            }else{
              return(x)
            }
          })
        }else{
          fc <- c()
        }
        inits[[ch]]$beta_ord_fix[[y]] <- auxpolr0$coefficients[fc]
        
        if(varying["c_ord"]){
          inits[[ch]]$c_ord[[y]] <- inits[[ch]]$a_ord[[y]] <- inits[[ch]]$pi_ord[[y]] <- list()
        }else{
          inits[[ch]]$c_ord[[y]] <- auxpolr0$zeta
          logits <- c(0, plogis(inits[[ch]]$c_ord[[y]]))
          inits[[ch]]$a_ord[[y]] <- log((logits[2:length(logits)] - logits[1:(length(logits)-1)])/(1-logits[length(logits)]))
          inits[[ch]]$pi_ord[[y]] <- exp(c(inits[[ch]]$a_ord[[y]], 0))/(sum(exp(c(inits[[ch]]$a_ord[[y]], 0))))
        }
        
        if(length(gcols[[y]]) > 0){
          gc <- sapply(gcols[[y]], function(x){
            xdash <- paste0("`", x, "`")
            if(is.element(xdash, names(auxpolr0$coefficients))){
              return(xdash)
            }else{
              return(x)
            }
          })
        }else{
          gc <- c()
        }
        inits[[ch]]$beta_ord[[y]] <- list()
        for(g in 1:G[ch]){
          if(length(unique(ydata[selection[[g]],y])) == Kord[y]+1){
            auxdata <- auxdata0[selection[[g]],,drop=FALSE]
          }else{
            auxdata <- auxdata0
          }
          auxdata[,y] <- factor(auxdata[,y])
          auxpolr <- tryCatch(polr(aux_form, data = auxdata), 
                              error=function(e) e, warning=function(w) w)
          # dowithall <- TRUE
          dowithall <- FALSE
          # if(is(auxpolr,"warning") || is(auxpolr,"error")){
          if(is(auxpolr,"error")){
            dowithall <- TRUE
          }else{
            if(is.null(auxpolr$zeta)){
              # can happen if rank-deficient (e.g. some category is not represented)
              dowithall <- TRUE
            }else{
              logits <- plogis(auxpolr$zeta)
              sdlogits <- sd(logits)
              if(is.na(sdlogits)){
                dowithall <- TRUE
              }else{
                if(auxpolr$convergence || sdlogits < 1e-14 || (sum(is.na(auxpolr$coefficients[gc]))>0)){
                  dowithall <- TRUE
                }
              }
            }
          }
          if(dowithall){
            auxpolr <- auxpolr0
          }
        
          inits[[ch]]$beta_ord[[y]][[g]] <- auxpolr$coefficients[gc]
          
          if(varying["c_ord"]){
            inits[[ch]]$c_ord[[y]][[g]] <- auxpolr$zeta 
            logits <- c(0, plogis(inits[[ch]]$c_ord[[y]][[g]]))
            inits[[ch]]$a_ord[[y]][[g]] <- log((logits[2:length(logits)] - logits[1:(length(logits)-1)])/(1-logits[length(logits)]))
            inits[[ch]]$pi_ord[[y]][[g]] <- exp(c(inits[[ch]]$a_ord[[y]][[g]], 0))/(sum(exp(c(inits[[ch]]$a_ord[[y]][[g]], 0))))
          }
        } # end for g in 1:G[ch]
      } # end for y in Ords
      
      # parameters connected to ordinal variables --> use multinomial logistic regression
      # library("nnet")
      inits[[ch]]$beta_cat <- inits[[ch]]$beta_cat_fix <- list()
      for(y in Cats){
        aux_form <- paste0(y, " ~ .")
        # Offsets are prohibited for categorical variables 
        # nnet requires multiple offsets (different for each category)
        # if(noff[y] > 0){
        #   aux_form <- paste0(aux_form, "+ offset(",formula[[y]]$offset,")")
        #  auxdata0 <- finX[,union(fcols[[y]], union(gcols[[y]], ocols[[y]]))]
        # }else{
          auxdata0 <- finX[,union(fcols[[y]], gcols[[y]])]
        # }
        # possibly standardized regressors
        auxdata0[,y] <- factor(ydata[,y]) # the first category is the baseline
        auxmult0 <- multinom(as.formula(aux_form), data = auxdata0, trace = FALSE)
        sumauxmult0 <- summary(auxmult0)
        if(length(fcols[[y]]) > 0){
          fc <- sapply(fcols[[y]], function(x){
            xdash <- paste0("`", x, "`")
            if(is.element(xdash, names(auxmult0$coefficients))){
              return(xdash)
            }else{
              return(x)
            }
          })
        }else{
          fc <- c()
        }
        inits[[ch]]$beta_cat_fix[[y]] <- t(sumauxmult0$coefficients[,fc])
        
        if(length(gcols[[y]]) > 0){
          gc <- sapply(gcols[[y]], function(x){
            xdash <- paste0("`", x, "`")
            if(is.element(xdash, names(auxmult0$coefficients))){
              return(xdash)
            }else{
              return(x)
            }
          })
        }else{
          gc <- c()
        }
        inits[[ch]]$beta_cat[[y]] <- list()
        for(g in 1:G[ch]){
          if(length(unique(ydata[selection[[g]],y])) == Kcat[y]+1){
            auxdata <- auxdata0[selection[[g]],,drop=FALSE]
          }else{
            auxdata <- auxdata0
          }
          auxdata[,y] <- factor(auxdata[,y]) # the first category is the baseline
          auxmult <- tryCatch(multinom(as.formula(aux_form), data = auxdata, trace = FALSE),
                              error=function(e) e, warning=function(w) w)
          # if(is(auxmult,"warning") || is(auxmult,"error")){
          if(is(auxmult,"error")){
            auxmult <- auxmult0
          }else{
            if(auxmult$convergence || (sum(is.na(coef(auxmult)[,gc]))>0)){
              auxmult <- auxmult0
            }
          }
          sumauxmult <- summary(auxmult)
          inits[[ch]]$beta_cat[[y]][[g]] <- t(sumauxmult$coefficients[,gc])
        }
        
      } # end for y in Cats
    
      # InvSigma, InvQ parameters
      # E InvSigma = E W(Q,nu_0) = nu_0 * Q
      # InvQ approx Inv(InvSigma/nu_0)
      if(totnran > 0){
        if(varying["InvQ"]){
          # so must be InvSigma
          inits[[ch]]$InvQ <- inits[[ch]]$InvSigma <- list()
          for(g in 1:G[ch]){
            inits[[ch]]$InvSigma[[g]] <- diag(totnran) / rgamma(totnran, 1/param$init_b_sd^2)
            inits[[ch]]$InvQ[[g]] <- chol2inv(chol(inits[[ch]]$InvSigma[[g]]/param$InvSigma_df))  
          } # end of for(k in 1:K)
        }else{
          if(varying["InvSigma"]){
            inits[[ch]]$InvSigma <- list()
            meanInvSigma <- matrix(0, ncol = totnran, nrow = totnran)
            for(g in 1:G[ch]){
              inits[[ch]]$InvSigma[[g]] <- diag(totnran) / rgamma(totnran, 1/param$init_b_sd^2)
              meanInvSigma <- meanInvSigma + inits[[ch]]$InvSigma[[g]]/G[ch]
            } 
            inits[[ch]]$InvQ <- chol2inv(chol(meanInvSigma/param$InvSigma_df))
          }else{
            inits[[ch]]$InvSigma <- diag(totnran) / rgamma(totnran, 1/param$init_b_sd^2)
            inits[[ch]]$InvQ <- chol2inv(chol(inits[[ch]]$InvSigma/param$InvSigma_df))
          } # end of if(varying["InvSigma"])
        } # end of else if(varying["InvQ"])
      
        # random effects b
        inits[[ch]]$b <- matrix(rnorm(n*totnran, mean = 0, sd = param$init_b_sd), 
                                nrow = n, ncol = totnran)
      }else{
        inits[[ch]]$InvQ <- inits[[ch]]$InvSigma <- inits[[ch]]$b <- NULL
      }
      #colnames(inits[[ch]]$b) <- rannames
      
      
      if(ch == nchains){message("Inits are ready.\n")}
      
    } # end for ch in 1:nchains
    
  } # end of missing(inits)
  
  
  
  
  ### Preparations for transfer from C to List output
  # creating settings matrix for all (calculable) parameters
  params <- c("beta_num_fix", "beta_num",
              "prec_num", "sd_num", "var_num",
              "beta_poi_fix", "beta_poi",
              "beta_bin_fix", "beta_bin",
              "beta_ord_fix", "beta_ord",
              "c_ord", "a_ord", "pi_ord",
              "beta_cat_fix", "beta_cat",
              "InvSigma", "Sigma", "sdSigma", "corSigma", "detInvSigma",
              "InvQ", "Q", "detInvQ",
              "b",
              "w", "ng",
              "loglik", "pUig", "U",
              "Gplus", "e0",
              "naY")
  ydepparams <- c(paste0("beta_", c("num", "poi", "bin", "ord", "cat"), "_fix"),
                  paste0("beta_", c("num", "poi", "bin", "ord", "cat")),
                  paste0(c("c", "a", "pi"), "_ord"))
  
  settings <- data.frame(save = save[params],
                         gspec = sapply(params, function(p){ifelse(is.na(varying[p]), FALSE, varying[p])}),
                         isy = FALSE,
                         ydepd1 = FALSE,
                         ydepd2 = FALSE,
                         ynums = FALSE,
                         ypois = FALSE,
                         ybins = FALSE,
                         yords = FALSE,
                         ycats = FALSE,
                         d2spec = FALSE,
                         iter = rep(iter, length(params)),
                         d1 = 0,
                         d2 = 0, 
                         BYROW = TRUE,
                         sym = FALSE,
                         diag = FALSE,
                         diagval = NA_real_,
                         D = 0)
  
  rownames(settings) <- params
  settings[ydepparams, "ydepd1"] <- TRUE
  settings[paste0("beta_", c("num", "poi", "bin", "ord", "cat")), "gspec"] <- TRUE
  #settings
  
  ## Individual changes
  # cluster allocation related parameters
  settings["w", c("gspec")] = c(TRUE)
  settings["ng", c("gspec")] <- c(TRUE)
  settings["U", c("d1", "D")] = c(n, 1)
  settings["pUig", c("gspec", "BYROW")] = c(TRUE, FALSE)
  settings["pUig", c("d1", "D")] = c(n, 1)
  
  # ydepparams have to be done separately
  # due to d1 varying on y
  settings["prec_num", c("isy", "ynums")] <- c(TRUE, TRUE)
  settings["sd_num", c("isy", "ynums", "gspec")] <- c(TRUE, TRUE, varying["prec_num"])
  settings["var_num", c("isy", "ynums", "gspec")] <- c(TRUE, TRUE, varying["prec_num"])
  settings["beta_num_fix", c("isy", "ynums")] <- c(TRUE,TRUE)
  settings["beta_num", c("isy", "ynums")] <- c(TRUE,TRUE)
  
  settings["beta_poi_fix", c("isy", "ypois")] <- c(TRUE,TRUE)
  settings["beta_poi", c("isy", "ypois")] <- c(TRUE,TRUE)
  
  settings["beta_bin_fix", c("isy", "ybins")] <- c(TRUE,TRUE)
  settings["beta_bin", c("isy", "ybins")] <- c(TRUE,TRUE)
  
  settings["beta_ord_fix", c("isy", "yords")] <- c(TRUE,TRUE)
  settings["beta_ord", c("isy", "yords")] <- c(TRUE,TRUE)
  settings["c_ord", c("isy", "yords")] <- c(TRUE,TRUE)
  settings["a_ord", c("gspec", "isy", "yords")] <- c(varying["c_ord"],TRUE,TRUE)
  settings["pi_ord", c("gspec", "isy", "yords")] <- c(varying["c_ord"],TRUE,TRUE)
  
  settings[c("beta_num_fix", "beta_num",
             "beta_poi_fix", "beta_poi", "beta_bin_fix", "beta_bin",
             "beta_ord_fix", "beta_ord", "c_ord", "a_ord", "pi_ord"), "D"] <- 1
  
  settings["beta_cat_fix", c("isy", "ydepd2", "ycats", "BYROW")] <- c(TRUE,TRUE,TRUE,TRUE)
  settings["beta_cat", c("isy", "ydepd2", "ycats", "BYROW")] <- c(TRUE,TRUE,TRUE,TRUE)
  settings[c("beta_cat_fix", "beta_cat"), "D"] <- 2
  
  yspecd1 <- yspecd2 <- d2spec <- list()
  # for(p in params){
  #   yspecd1[[p]] <- yspecd2[[p]] <- list()
  # }
  yspecd1$a_ord <- yspecd1$c_ord <- Kord
  yspecd1$pi_ord <- Kord+1
  yspecd1$beta_num_fix <- nfix[Nums]
  yspecd1$beta_poi_fix <- nfix[Pois]
  yspecd1$beta_bin_fix <- nfix[Bins]
  yspecd1$beta_ord_fix <- nfix[Ords]
  yspecd1$beta_cat_fix <- nfix[Cats]
  yspecd1$beta_num <- ngrp[Nums]
  yspecd1$beta_poi <- ngrp[Pois]
  yspecd1$beta_bin <- ngrp[Bins]
  yspecd1$beta_ord <- ngrp[Ords]
  yspecd1$beta_cat <- ngrp[Cats]
  yspecd2$beta_cat_fix <- yspecd2$beta_cat <- Kcat
  
  # Sigma parameters
  settings["InvSigma", c("sym", "diag")] <- c(TRUE,TRUE)
  settings["InvSigma", c("d1", "d2", "D")] <- c(totnran,totnran,2)
  settings["Sigma", c("gspec", "sym", "diag")] <- c(varying["InvSigma"],TRUE,TRUE)
  settings["Sigma", c("d1", "d2", "D")] <- c(totnran,totnran,2)
  settings["sdSigma", c("gspec")] <- c(varying["InvSigma"])
  settings["sdSigma", c("d1", "D")] <- c(totnran, 1)
  settings["corSigma", c("gspec", "sym", "diag")] <- c(varying["InvSigma"],TRUE,FALSE)
  settings["corSigma", c("d1", "d2", "D")] <- c(totnran,totnran,2)
  settings["corSigma", c("diagval")] <- c(1.0)
  settings["detInvSigma", c("gspec")] <- c(varying["InvSigma"])
  
  # Q parameters
  settings["InvQ", c("sym", "diag")] <- c(TRUE,TRUE)
  settings["InvQ", c("d1", "d2", "D")] <- c(totnran,totnran,2)
  settings["Q", c("gspec", "sym", "diag")] <- c(varying["InvQ"],TRUE,TRUE)
  settings["Q", c("d1", "d2", "D")] <- c(totnran,totnran,2)
  settings["detInvQ", c("gspec")] <- c(varying["InvQ"])
  
  # random effects b
  settings["b", c("BYROW")] <- c(TRUE)
  settings["b", c("d1", "d2", "D")] <- c(n, totnran, 2)
  
  # sparse clusters parameters

  
  ### Preparation of parameters for Gibbs sampler in C
  ## Data
  # First column is going to be the id variable (0-th column)
  cId <- numbered_unique_ids[as.character(data[,id])] - 1
  # -1 is there for C which works better with number beginning with 0
  # other columns  (beginning with 1st column)
  cY <- numeric()
  for(y in Ys){
    if(is.element(y, Cats)){
      # Change the first category to be the last, otherwise preserve the order
      auxy <- ydata[,y]
      auxy[auxy==0] <- Kcat[y]+1
      auxy <- auxy - 1
      cY <- c(cY, auxy)
    }else{
      cY <- c(cY, ydata[,y])
    }
    #cY <- c(cY, as.numeric(as.character(data[,y])))
  }
  cisYna <- is.na(cY)
  cY[cisYna] <- 0 # NA is not sent to C function
  
  settings["naY", c("d1", "D")] <- 
    c(sum(cisYna), 1)
  settings["naY", c("ydepd1", "isy", "ynums", "ypois", "ybins", "yords", "ycats")] <-
    rep(TRUE, 7)
  yspecd1$naY <- apply(ydata[,Ys,drop=FALSE], 2, function(x){sum(is.na(x))})
  
  cX <- numeric()
  # take only needed columns
  Xcolnums <- 1:length(Xcolnames)
  names(Xcolnums) <- Xcolnames
  for(x in Xcolnames){
    cX <- c(cX, finX[,x])
  } # no need for id
  
  # formula
  cFormulaF <- cFormulaG <- cFormulaR <- cFormulaO <- numeric()
  for(y in Ys){
    cFormulaF <- c(cFormulaF, Xcolnums[lfixnames[[y]]])
    cFormulaG <- c(cFormulaG, Xcolnums[lgrpnames[[y]]])
    cFormulaR <- c(cFormulaR, Xcolnums[lrannames[[y]]])
    cFormulaO <- c(cFormulaO, Xcolnums[loffnames[[y]]])
  }
  cFormulaF <- cFormulaF - 1 # index in C
  cFormulaG <- cFormulaG - 1 # index in C
  cFormulaR <- cFormulaR - 1 # index in C
  cFormulaO <- cFormulaO - 1 # index in C
  cnfix <- as.numeric(nfix) # number of FIXED  regressors with variables y
  cngrp <- as.numeric(ngrp) # number of GROUP-SPECIFIC FIXED  regressors with variables y
  cnran <- as.numeric(nran) # number of RANDOM regressors with variables y
  cnoff <- as.numeric(noff) # number of OFFSETS regressors with variables y
  # dims - in the following order:
  cdims <- 
    c(sum(nfix[Nums]), sum(ngrp[Nums]), #"beta_num_fix", "beta_num",
      nY["Nums"], nY["Nums"], nY["Nums"], #"prec_num", "sd_num", "var_num",
      sum(nfix[Pois]), sum(ngrp[Pois]), #"beta_poi_fix", "beta_poi",
      sum(nfix[Bins]), sum(ngrp[Bins]), #"beta_bin_fix", "beta_bin",
      sum(nfix[Ords]), sum(ngrp[Ords]), #"beta_ord_fix", "beta_ord",
      sum(Kord), sum(Kord), sum(Kord+1), #"a_ord", "c_ord", "pi_ord",
      sum(nfix[Cats]*Kcat), sum(ngrp[Cats]*Kcat), #"beta_cat_fix", "beta_cat",
      totnran*(totnran+1)/2, totnran*(totnran+1)/2, totnran, totnran*(totnran-1)/2, as.numeric(totnran>0),
      #"InvSigma", "Sigma", "sdSigma", "corSigma", "detInvSigma",
      totnran*(totnran+1)/2, totnran*(totnran+1)/2, as.numeric(totnran>0), #"InvQ", "Q", "detInvQ",
      n*totnran, #"b",
      1, 1, #"w", "ng",
      1, n, n, #"loglik", "pUig", "U",
      1, 1,  #"Gplus", "e0"
      sum(cisYna)
    )
  names(cdims) <- params 
  settings$dims <- cdims[params]
  
  dimswithG <- matrix(0, nrow = length(params), ncol = nchains)
  rownames(dimswithG) <- params
  for(ch in 1:nchains){
    dimswithG[,ch] <- cdims * ((!settings$gspec)*1 + (settings$gspec)*G[ch])
  }
  
  # cparam - created in specific order (user is not required to have the same order)
  cparam <- list(InvSigma_df = param$InvSigma_df, # for I-W prior of Sigma
                 InvQ_df = param$InvQ_df, # for I-W prior of Q
                 prec_num_shp=param$prec_num_shp, 
                 prec_num_rte=param$prec_num_rte, # gamma prior for precs
                 # standard deviations for beta parameters depending on type and group-specificity
                 beta_num_fix_sd = param$beta_num_fix_sd, 
                 beta_num_sd = param$beta_num_sd,
                 beta_poi_fix_sd = param$beta_poi_fix_sd,
                 beta_poi_sd = param$beta_poi_sd,
                 beta_bin_fix_sd = param$beta_bin_fix_sd,
                 beta_bin_sd = param$beta_bin_sd,
                 beta_ord_fix_sd = param$beta_ord_fix_sd,
                 beta_ord_sd = param$beta_ord_sd,
                 api_prior = param$api_prior,
                 beta_cat_fix_sd = param$beta_cat_fix_sd,
                 beta_cat_sd = param$beta_cat_sd,
                 # gamma prior hyperparameters for e_0
                 e0_shp = param$e0_shp, 
                 e0_rte = param$e0_rte)
  cparam$InvV <- param$InvV[upper.tri(param$InvV, diag = TRUE)]
  # order is df, prec_num_shp,_rte, beta_sd, e0_shp,_rte, (all [1])
  # InvV [totnran*(totnran+1)/2]
  cparam <- unlist(cparam) # will be delivered as vector
  
  # csave - what parameters should be returned by C function
  csave <- save
  csave["U"] <- TRUE # in order to perform U-based clustering
  
  # ctuning - tuning in a given order
  ctuning <- list()
  ctuning$integer <- list(
    freq_proposal_update = tuning$integer$freq_proposal_update,
    times_proposal = tuning$integer$times_proposal,
    maxiter = tuning$integer$maxiter,
    maxnrep = tuning$integer$maxnrep,
    kspec_bi_cat = tuning$integer$kspec_bi_cat
  )
  ctuning$double <- list(
    const_proposal_beta_poi_fix = tuning$double$const_proposal_beta_poi_fix,
    const_proposal_beta_poi = tuning$double$const_proposal_beta_poi,
    const_proposal_beta_bin_fix = tuning$double$const_proposal_beta_bin_fix,
    const_proposal_beta_bin = tuning$double$const_proposal_beta_bin,
    const_proposal_beta_ord_fix = tuning$double$const_proposal_beta_ord_fix,
    const_proposal_beta_ord = tuning$double$const_proposal_beta_ord,
    const_proposal_beta_cat_fix = tuning$double$const_proposal_beta_cat_fix,
    const_proposal_beta_cat = tuning$double$const_proposal_beta_cat,
    const_proposal_a_ord = tuning$double$const_proposal_a_ord,
    const_proposal_b = tuning$double$const_proposal_b,
    const_proposal_e0 = tuning$double$const_proposal_e0,
    tolerance = tuning$double$tolerance
  )
  
  # list of last generated states
  last <- list()
  Usamples <- list()
  
  message("Settings ready, about to start for ch in 1:nchains.\n")
  
  nparams <- numeric(nchains)
  cnames <- list()
  mcmc$param_names <- list()
  lsettings <- list()
  
  for(ch in 1:nchains){
    lsettings[[ch]] <- settings 
    lsettings[[ch]]$G <- G[ch]
    lsettings[[ch]]$dimswithG <- dimswithG[params,ch]
    
    nparams[ch] <- sum(dimswithG[,ch] * lsettings[[ch]]$save)
    cnames[[ch]] <- character(1+nparams[ch])
    cnames[[ch]][1] <- c("m")
    index <- 1
    mcmc$param_names[[ch]] <- list()
    for(p in params){
      if(lsettings[[ch]][p,"save"] & lsettings[[ch]][p,"dims"] > 0){
        aux = from_C_to_matrix(values = rep(0, dimswithG[p,ch]),
                               p = p,
                               settings = lsettings[[ch]],
                               yspecd1 = yspecd1[[p]],
                               yspecd2 = yspecd2[[p]],
                               family = sorted_family)
        mcmc$param_names[[ch]][[p]] <- colnames(aux)
        cnames[[ch]][(index+1):(index+dimswithG[p,ch])] <- colnames(aux)
        index <- index + dimswithG[p,ch]
      }else{
        mcmc$param_names[[ch]][[p]] <- c()
      }
    }
  }

  mcmc$draws <- list()
  for(ch in 1:nchains){
    if(howsave == "data.frame"){
      mcmc$draws[[ch]] <- matrix(NA_integer_, nrow = iter, ncol = 1+nparams[ch])
      mcmc$draws[[ch]][,1] = 1:iter
      colnames(mcmc$draws[[ch]]) <- cnames[[ch]]
      mcmc$draws[[ch]] = as.data.frame(mcmc$draws[[ch]])
    }else{
      mcmc$draws[[ch]] <- list()
      # mcmc$draws[[ch]]$m <- 1:iter
    }
  }

  ### Now prepare inits for each chain
  # and generate chains
  for(ch in 1:nchains){
    # cstore
    cstore <- list()
    for(p in params){
      if(p=="U" | p=="Gplus" | p=="ng"){
        # integer variables
        if(lsettings[[ch]][p,"save"]){
          cstore[[p]] <- integer(lsettings[[ch]][p,"iter"] * dimswithG[p,ch])
        }else{
          cstore[[p]] <- as.integer(0)
        }
      }else{
        # double variables
        if(lsettings[[ch]][p,"save"]){
          cstore[[p]] <- double(lsettings[[ch]][p,"iter"] * dimswithG[p,ch])
        }else{
          cstore[[p]] <- as.double(0)
        }
      }
    }

    ## initial values for chain ch
    initsch <- inits[[ch]]
    # cinits
    cinits <- list()
    cinits$w <- initsch$w
    cinits$e0 <- initsch$e0
    cinits$U <- initsch$U-1
    cinits$pUig <- c(initsch$pUig)

    cinits$prec_num <- numeric()
    if(varying["prec_num"]){
      for(g in 1:G[ch]){
        for(y in Nums){
          cinits$prec_num <- c(cinits$prec_num, initsch$prec_num[[y]][[g]])
        }
      }
    }else{
      cinits$prec_num <- unlist(initsch$prec_num)
    }

    cinits$beta_num_fix <- unlist(initsch$beta_num_fix)
    cinits$beta_num <- numeric()
    for(g in 1:G[ch]){
      for(y in Nums){
        cinits$beta_num <- c(cinits$beta_num, initsch$beta_num[[y]][[g]])
      }
    }

    cinits$beta_poi_fix <- unlist(initsch$beta_poi_fix)
    cinits$beta_poi <- numeric()
    for(g in 1:G[ch]){
      for(y in Pois){
        cinits$beta_poi <- c(cinits$beta_poi, initsch$beta_poi[[y]][[g]])
      }
    }

    cinits$beta_bin_fix <- unlist(initsch$beta_bin_fix)
    cinits$beta_bin <- numeric()
    for(g in 1:G[ch]){
      for(y in Bins){
        cinits$beta_bin <- c(cinits$beta_bin, initsch$beta_bin[[y]][[g]])
      }
    }

    cinits$beta_ord_fix <- unlist(initsch$beta_ord_fix)
    cinits$beta_ord <- numeric()
    for(g in 1:G[ch]){
      for(y in Ords){
        cinits$beta_ord <- c(cinits$beta_ord, initsch$beta_ord[[y]][[g]])
      }
    }

    cinits$c_ord <- numeric()
    if(varying["c_ord"]){
      for(g in 1:G[ch]){
        for(y in Ords){
          cinits$c_ord <- c(cinits$c_ord, initsch$c_ord[[y]][[g]])
        }
      }
    }else{
      cinits$c_ord <- unlist(initsch$c_ord)
    }

    cinits$a_ord <- numeric()
    if(varying["c_ord"]){
      for(g in 1:G[ch]){
        for(y in Ords){
          cinits$a_ord <- c(cinits$a_ord, initsch$a_ord[[y]][[g]])
        }
      }
    }else{
      cinits$a_ord <- unlist(initsch$a_ord)
    }

    cinits$pi_ord <- numeric()
    if(varying["c_ord"]){
      for(g in 1:G[ch]){
        for(y in Ords){
          cinits$pi_ord <- c(cinits$pi_ord, initsch$pi_ord[[y]][[g]])
        }
      }
    }else{
      cinits$pi_ord <- unlist(initsch$pi_ord)
    }

    cinits$beta_cat_fix <- unlist(initsch$beta_cat_fix)
    cinits$beta_cat <- numeric()
    for(g in 1:G[ch]){
      for(y in Cats){
        cinits$beta_cat <- c(cinits$beta_cat, initsch$beta_cat[[y]][[g]])
      }
    }

    # first [[1]], then regressor 1, then rows for different k values

    if(totnran > 0){
      if(varying["InvSigma"]){
        InvSigma2 <- list()
        for(g in 1:G[ch]){
          InvSigma2[[g]] <- initsch$InvSigma[[g]][upper.tri(initsch$InvSigma[[g]], diag = TRUE)]
        }
        cinits$InvSigma <- unlist(InvSigma2)
      }else{
        cinits$InvSigma <- initsch$InvSigma[upper.tri(initsch$InvSigma, diag = TRUE)]
      }
      if(varying["InvQ"]){
        InvQ2 <- list()
        for(g in 1:G[ch]){
          InvQ2[[g]] <- initsch$InvQ[[g]][upper.tri(initsch$InvQ[[g]], diag = TRUE)]
        }
        cinits$InvQ <- unlist(InvQ2)
      }else{
        cinits$InvQ <- initsch$InvQ[upper.tri(initsch$InvQ, diag = TRUE)]
      }
      cinits$b <- c(t(initsch$b))
    }
    cinits$naY <- rep(0.0, dimswithG["naY",ch]) # does not matter what --> it is updated as first

    cinits <- unlist(cinits) # will be delivered as a vector
    #which(is.na(cinits))
    #cinits_U <- initsch$U - 1 # need values 0, ..., G[ch]-1

    message(paste0("Inits for chain ", ch, " are ready.\n"))
    message("Triggering C function.\n")
    #summary(cinits)

    #dyn.load(paste0(ROOT, "Cfun/Metropolis_within_Gibbs_MBC_NumPoiBinOrdCat.dll"))
    #dyn.unload(paste0(ROOT, "Cfun/Metropolis_within_Gibbs_MBC_NumPoiBinOrdCat.dll"))
    #save.image(paste0(ROOT, "image.RData"))
    #load(paste0(ROOT, "image.RData"))

    #system.time(
    cmcmc <-
      .C(C_Metropolis_within_Gibbs_MBC_NumPoiBinOrdCat,
         Id        = as.integer(cId),
         Y         = as.double(cY),
         isYna     = as.integer(cisYna),
         X         = as.double(cX),
         varying   = as.integer(varying),
         save      = as.integer(unlist(csave)),
         vecparam  = as.double(cparam), # passed as vector of values
         vecinits  = as.double(cinits), # passed as vector of double values (except U)
         veclast   = double(length(cinits)), # passed as vector of double values (except U)
         # parameters describing dimensions
         chain     = as.integer(ch), # number of the chain
         G         = as.integer(G[ch]), # number of components for the current chain
         iter      = as.integer(iter), # total number of generated states
         N         = as.integer(N), # total number of observations
         n         = as.integer(n), # total number of subjects (different ids in the dataset)
         nY        = as.integer(nY), # 4 numbers: counts of Nums,  Bins, Ords and Cats variables
         FormulaF  = as.integer(cFormulaF), # numbers of columns of X that should be used for FIXED  effects of modelled responses
         FormulaG  = as.integer(cFormulaG), # numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
         FormulaR  = as.integer(cFormulaR), # numbers of columns of X that should be used for RANDOM effects of modelled responses
         FormulaO  = as.integer(cFormulaO), # numbers of columns of X that should be used for OFFSET effects of modelled responses
         nfix      = as.integer(cnfix),
         ngrp      = as.integer(cngrp),
         nran      = as.integer(cnran),
         noff      = as.integer(cnoff),
         Kord      = as.integer(Kord), # the counts of categories of ordinal variables (-1)
         Kcat      = as.integer(Kcat), # the counts of categories of categorical variables (-1)
         dims      = as.integer(cdims), # the length of subarray that corresponds to one state (disected by various parameters)
         dimswithG = as.integer(dimswithG[,ch]), # the length of subarray that corresponds to one state
         # (disected by various parameters, also multiplication by K incorporated when such parameters is class-specific)
         # arrays to store generated states
         beta_num_fix= cstore$beta_num_fix,
         beta_num    = cstore$beta_num,
         prec_num    = cstore$prec_num,
         sd_num      = cstore$sd_num,
         var_num     = cstore$var_num,
         beta_poi_fix= cstore$beta_poi_fix,
         beta_poi    = cstore$beta_poi,
         beta_bin_fix= cstore$beta_bin_fix,
         beta_bin    = cstore$beta_bin,
         beta_ord_fix= cstore$beta_ord_fix,
         beta_ord    = cstore$beta_ord,
         a_ord       = cstore$a_ord,
         c_ord       = cstore$c_ord,
         pi_ord      = cstore$pi_ord,
         beta_cat_fix= cstore$beta_cat_fix,
         beta_cat    = cstore$beta_cat,
         InvSigma    = cstore$InvSigma,
         Sigma       = cstore$Sigma,
         sdSigma     = cstore$sdSigma,
         corSigma    = cstore$corSigma,
         detInvSigma = cstore$detInvSigma,
         InvQ        = cstore$InvQ,
         Q           = cstore$Q,
         detInvQ     = cstore$detInvQ,
         b           = cstore$b,
         w           = cstore$w,
         ng          = cstore$ng,
         loglik      = cstore$loglik,
         pUig        = cstore$pUig,
         U           = cstore$U,
         Gplus       = cstore$Gplus,
         e0          = cstore$e0,
         naY         = cstore$naY,
         # Tuning parameters
         vectuningdouble  = as.double(unlist(ctuning$double)),
         vectuninginteger = as.integer(unlist(ctuning$integer))
      )
    #)
    message(paste0("Sampling of chain ", ch, " is completed.\n"))

    ### reconstruction of last state from cmcmc$last_U and cmcmc$veclast
    last[[ch]] <- list()
    # order of construction of inits: "w","U","pUig","prec_num",
    #  "beta_num","beta_bin","beta_ord","c_ord","a_ord","beta_cat"
    #  "InvSigma","InvQ","b"
    # all last values are returned on the scale used in C --> no rescaling needed
    last[[ch]]$w <- cmcmc$veclast[1:(lastcopied <- dimswithG["w",ch])]
    last[[ch]]$e0 <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + 1)]
    last[[ch]]$U <- 1+cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + n)]
    last[[ch]]$pUig <- matrix(cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + n*G[ch])],
                              n, G[ch], byrow = lsettings[[ch]]["pUig", "BYROW"])

   # prec_num
    last[[ch]]$prec_num <- list()
    if(varying["prec_num"]){
      for(y in Nums){
        last[[ch]]$prec_num[[y]] <- list()
      }
      for(g in 1:G[ch]){
        for(y in Nums){
          last[[ch]]$prec_num[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + 1)]
        }
      }
    }else{
      for(y in Nums){
        last[[ch]]$prec_num[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + 1)]
      }
    }

    # beta_num
    last[[ch]]$beta_num_fix <- list()
    for(y in Nums){
      if(nfix[y]>0){
        last[[ch]]$beta_num_fix[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + nfix[y])]
      }else{
        last[[ch]]$beta_num_fix[[y]] <- numeric()
      }
    }
    last[[ch]]$beta_num <- list()
    for(y in Nums){
      last[[ch]]$beta_num[[y]] <- list()
    }
    for(g in 1:G[ch]){
      for(y in Nums){
        if(ngrp[y]>0){
          last[[ch]]$beta_num[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + ngrp[y])]
        }else{
          last[[ch]]$beta_num[[y]][[g]] <- numeric()
        }
      }
    }

    # beta_poi
    last[[ch]]$beta_poi_fix <- list()
    for(y in Pois){
      if(nfix[y]>0){
        last[[ch]]$beta_poi_fix[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + nfix[y])]
      }else{
        last[[ch]]$beta_poi_fix[[y]] <- numeric()
      }
    }
    last[[ch]]$beta_poi <- list()
    for(y in Pois){
      last[[ch]]$beta_poi[[y]] <- list()
    }
    for(g in 1:G[ch]){
      for(y in Pois){
        if(ngrp[y]>0){
          last[[ch]]$beta_poi[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + ngrp[y])]
        }else{
          last[[ch]]$beta_poi[[y]][[g]] <- numeric()
        }
      }
    }

    # beta_bin
    last[[ch]]$beta_bin_fix <- list()
    for(y in Bins){
      if(nfix[y]>0){
        last[[ch]]$beta_bin_fix[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + nfix[y])]
      }else{
        last[[ch]]$beta_bin_fix[[y]] <- numeric()
      }
    }
    last[[ch]]$beta_bin <- list()
    for(y in Bins){
      last[[ch]]$beta_bin[[y]] <- list()
    }
    for(g in 1:G[ch]){
      for(y in Bins){
        if(ngrp[y]>0){
          last[[ch]]$beta_bin[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + ngrp[y])]
        }else{
          last[[ch]]$beta_bin[[y]][[g]] <- numeric()
        }
      }
    }

    # beta_ord
    last[[ch]]$beta_ord_fix <- list()
    for(y in Ords){
      if(nfix[y]>0){
        last[[ch]]$beta_ord_fix[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + nfix[y])]
      }else{
        last[[ch]]$beta_ord_fix[[y]] <- numeric()
      }
    }
    last[[ch]]$beta_ord <- list()
    for(y in Ords){
      last[[ch]]$beta_ord[[y]] <- list()
    }
    for(g in 1:G[ch]){
      for(y in Ords){
        if(ngrp[y]>0){
          last[[ch]]$beta_ord[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + ngrp[y])]
        }else{
          last[[ch]]$beta_ord[[y]][[g]] <- numeric()
        }
      }
    }

    # c_ord
    if(varying["c_ord"]){
      last[[ch]]$c_ord <- list()
      for(y in Ords){
        last[[ch]]$c_ord[[y]] <- list()
      }
      for(g in 1:G[ch]){
        for(y in Ords){
          last[[ch]]$c_ord[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + Kord[y])]
        }
      }
    }else{
      last[[ch]]$c_ord <- list()
      for(y in Ords){
        last[[ch]]$c_ord[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + Kord[y])]
      }
    }
    # a_ord
    if(varying["c_ord"]){
      last[[ch]]$a_ord <- list()
      for(y in Ords){
        last[[ch]]$a_ord[[y]] <- list()
      }
      for(g in 1:G[ch]){
        for(y in Ords){
          last[[ch]]$a_ord[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + Kord[y])]
        }
      }
    }else{
      last[[ch]]$a_ord <- list()
      for(y in Ords){
        last[[ch]]$a_ord[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + Kord[y])]
      }
    }
    # pi_ord
    if(varying["c_ord"]){
      last[[ch]]$pi_ord <- list()
      for(y in Ords){
        last[[ch]]$pi_ord[[y]] <- list()
      }
      for(g in 1:G[ch]){
        for(y in Ords){
          last[[ch]]$pi_ord[[y]][[g]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + Kord[y]+1)]
        }
      }
    }else{
      last[[ch]]$pi_ord <- list()
      for(y in Ords){
        last[[ch]]$pi_ord[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + Kord[y]+1)]
      }
    }

    # beta_cat
    last[[ch]]$beta_cat_fix <- list()
    for(y in Cats){
      if(nfix[y]>0){
        last[[ch]]$beta_cat_fix[[y]] <- matrix(cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + nfix[y]*Kcat[y])],
                                               nrow = nfix[y])
      }else{
        last[[ch]]$beta_cat_fix[[y]] <- numeric()
      }
    }
    last[[ch]]$beta_cat <- list()
    for(y in Cats){
      last[[ch]]$beta_cat[[y]] <- list()
    }
    for(g in 1:G[ch]){
      for(y in Cats){
        if(ngrp[y]>0){
          last[[ch]]$beta_cat[[y]][[g]] <- matrix(cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + ngrp[y]*Kcat[y])],
                                                  nrow = ngrp[y])
        }else{
          last[[ch]]$beta_cat[[y]][[g]] <- numeric()
        }
      }
    }

    # random-effects related parameters (only if random effects are present)
    if(totnran > 0){
      # InvSigma
      if(varying["InvSigma"]){
        last[[ch]]$InvSigma <- list()
        for(g in 1:G[ch]){
          auxmatrix <- matrix(0, totnran, totnran)
          pomvec <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + cdims["InvSigma"])]
          auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
          auxmatrix <- t(auxmatrix)
          auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
          last[[ch]]$InvSigma[[g]] <- auxmatrix
        }
      }else{
        auxmatrix <- matrix(0, totnran, totnran)
        pomvec <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + cdims["InvSigma"])]
        auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
        auxmatrix <- t(auxmatrix)
        auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
        last[[ch]]$InvSigma <- auxmatrix
      }
      # InvQ
      if(varying["InvQ"]){
        last[[ch]]$InvQ <- list()
        for(g in 1:G[ch]){
          auxmatrix <- matrix(0, totnran, totnran)
          pomvec <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + cdims["InvQ"])]
          auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
          auxmatrix <- t(auxmatrix)
          auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
          last[[ch]]$InvQ[[g]] <- auxmatrix
        }
      }else{
        auxmatrix <- matrix(0, totnran, totnran)
        pomvec <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + cdims["InvQ"])]
        auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
        auxmatrix <- t(auxmatrix)
        auxmatrix[upper.tri(auxmatrix, diag = TRUE)] <- pomvec
        last[[ch]]$InvQ <- auxmatrix
      }
      # b
      last[[ch]]$b <- matrix(cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + dimswithG["b",ch])],
                             n, totnran, byrow = TRUE)
    }

    if(cdims["naY"]>0){
      last[[ch]]$naY <- list()
      if(varying["naY"]){
        for(g in 1:G[ch]){
          last[[ch]]$naY[[g]] <- list()
          for(y in Ys){
            if(yspecd1$naY[y]>0){
              last[[ch]]$naY[[g]][[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + yspecd1$naY[y])]
            }else{
              last[[ch]]$naY[[g]][[y]] <- numeric()
            }
          }
        }
      }else{
        for(y in Ys){
          if(yspecd1$naY[y]>0){
            last[[ch]]$naY[[y]] <- cmcmc$veclast[(lastcopied + 1):(lastcopied <- lastcopied + yspecd1$naY[y])]
          }else{
            last[[ch]]$naY[[y]] <- numeric()
          }
        }
      }
    }
    message(paste0("Saving the last state of chain ", ch, " is completed.\n"))

    ### Saving results as structured list (converted from cmcmc)
    # uses previously created matrix settings
    if(howsave == "cmcmc"){
      mcmc$draws[[ch]] <- cmcmc
      Usamples[[ch]] <- 1 + cmcmc[["U"]]
    }

    if(howsave == "list"){
      # results will be returned in structured list - the same way as original R function
      chain <- list()
      chain$m <- 1:iter
      for(p in params){
        if(p == "U"){
            cmcmc[[p]] = Usamples[[ch]] = 1 + cmcmc[[p]] # to make cluster allocations in [1, ..., G[ch]]
          }
        if(lsettings[[ch]][p, "save"] & lsettings[[ch]][p,"dims"] > 0){
          chain[[p]] <- from_C_to_list(values = cmcmc[[p]],
                                       p = p,
                                       settings = lsettings[[ch]],
                                       yspecd1 = yspecd1[[p]],
                                       yspecd2 = yspecd2[[p]],
                                       family = sorted_family)
          if((p == "naY") & (length(Cats) > 0)){
            # C function believes that the last category is the baseline
            for(y in Cats){
              if(lsettings[[ch]][p,"gspec"]){
                # group specific
                for(g in 1:mcmc$G[ch]){
                  aux <- chain[[p]][[g]][[y]] + 1
                  aux[aux == Kcat[y]+1] <- 0
                  chain[[p]][[g]][[y]] <- aux
                }
              }else{
                aux <- chain[[p]][[y]] + 1
                aux[aux == Kcat[y]+1] <- 0
                chain[[p]][[y]] <- aux
              }
            }
          }
        }
      }
      if(standardize){
        mcmc$draws[[ch]] <- rescale_list(chain, lsettings[[ch]], sorted_family,
                                         lfixnames, lgrpnames, lrannames,
                                         Kord, Kcat, centers, scales)
      }else{
        mcmc$draws[[ch]] <- chain
      }
      message(paste0("Saving chain ", ch, " into lists is completed.\n"))
    }

    if(howsave == "data.frame"){
      # results will be returned in data.frame (row = state number, col = variable)
      # each chain will be saved in different list
      for(p in params){
        if(p == "U"){
          cmcmc[[p]] = Usamples[[ch]] = 1 + cmcmc[[p]] # to make cluster allocations in [1, ..., G[ch]]
        }
        if(lsettings[[ch]][p,"save"] & lsettings[[ch]][p,"dims"] > 0){
          aux = from_C_to_matrix(values = cmcmc[[p]],
                                 p = p,
                                 settings = lsettings[[ch]],
                                 yspecd1 = yspecd1[[p]],
                                 yspecd2 = yspecd2[[p]],
                                 family = sorted_family)
          if((p == "naY") & (length(Cats) > 0)){
            # C function believes that the last category is the baseline
            for(y in Cats){
              naycols <- grep(paste0("^naY_",y), colnames(aux))
              aux[,naycols] <- aux[,naycols] + 1
              aux[aux == Kcat[y]+1, naycols] <- 0
            }
          }
          mcmc$draws[[ch]][, colnames(aux)] <- aux
        }

      }
      if(standardize){
        mcmc$draws[[ch]] <- rescale_matrix(mcmc$draws[[ch]], lsettings[[ch]], sorted_family,
                                           lfixnames, lgrpnames, lrannames,
                                           Kord, Kcat, centers, scales)
      }
      message(paste0("Saving chain ", ch, " is completed.\n"))
    }
  } # end of chain
  message("All chains have been sampled.\n")


  ## U-based clustering
  fun0 <- function(U, ch){
    as.numeric(tabulate(U, G[ch]) > 0)
  }

  fun1 <- function(U, ch){
    which.max(tabulate(U, G[ch]))
  }

  fun2 <- function(U, ch){
    max(tabulate(U), na.rm = TRUE) / iter
  }

  clusters <- iterations <- list()
  modeGplus <- numeric(nchains)
  clusteringU <- matrix(NA_integer_, nrow = as.numeric(n), ncol = nchains)
  certaintyU <- matrix(NA_real_, nrow = as.numeric(n), ncol = nchains)
  for(ch in 1:nchains){
    iterations[[ch]] <- mcmc$draws[[ch]]$m
    # find modeGplus - the most common Gplus value
    tab <- table(mcmc$draws[[ch]]$Gplus)
    modeGplus[ch] <- as.numeric(names(tab)[which.max(tab)])
    # print(modeGplus[ch])
    # U samples
    Usamples[[ch]] <- from_C_to_matrix(values = Usamples[[ch]],
                                       p = "U",
                                       settings = lsettings[[ch]],
                                       family = sorted_family)
    # TAB <- table(unlist(Usamples[[ch]]))
    # TAB <- TAB[order(TAB, decreasing = TRUE)]
    # clusters[[ch]] <- sort(as.numeric(names(TAB)[1:modeGplus[ch]]))
    # Matrix indicating which cluster in which iteration is nonempty
    nonempties <- matrix(apply(Usamples[[ch]], 1, fun0, ch=ch),
                         nrow = G[ch])
    # Only the top modeGplus in non-empty frequency are considered
    sumnonempt <- apply(nonempties, 1, sum)
    names(sumnonempt) <- as.character(1:G[ch])
    sumnonempt <- sumnonempt[order(sumnonempt, decreasing = TRUE)]
    clusters[[ch]] <- sort(as.numeric(names(sumnonempt)[1:modeGplus[ch]]))
    #print(clusters[[ch]])
    #print(table(Usamples[[ch]][,122]))

    # ccU <- apply(Usamples[[ch]], 2, fun, ch=ch)
    # print(ccU)
    # clusteringU[,ch] <- ccU[1, ]
    # certaintyU[,ch] <- ccU[2, ]
    #print(dim(Usamples[[ch]]))
    cgU <- apply(Usamples[[ch]], 2, fun1, ch=ch)
    #print(cgU['U[121]'])
    #print(cgU['U[122]'])
    #print(cgU['U[123]'])
    #print(dim(cgU))
    cyU <- apply(Usamples[[ch]], 2, fun2, ch=ch)
    #print(cyU)
    #print(dim(cyU))
    clusteringU[,ch] <- cgU
    certaintyU[,ch] <- cyU
  }
  # units with frequency ratio smaller than threshold remain unclassified (group 0)
  # clusteringU[certaintyU < threshold] <- 0
  rownames(clusteringU) <- rownames(certaintyU) <- unique_ids
  mcmc$Usamples <- Usamples


  mcmc$modeGplus <- modeGplus
  mcmc$clusters <- clusters
  # Are the clusters for all chains the same?
  sameclusters <- TRUE
  for(ch in seq_len(nchains)[-1]){
    sameclusters <- (sameclusters & setequal(mcmc$clusters[[1]], mcmc$clusters[[ch]]))
  }
  mcmc$sameclusters <- sameclusters

  mcmc$clustering <- clusteringU
  mcmc$certainty <- certaintyU
  
  
  mcmc$inits <- inits
  mcmc$last <- last
  mcmc$InitType <- ifelse(initsgiven, "given", "created")

  mcmc$call <- c("###------------------------------------------------------------###\n")
  mcmc$call <- paste0(mcmc$call, "### Model Based Clustering for Generalized Linear Mixed Models ###\n")
  mcmc$call <- paste0(mcmc$call, "###------------------------------------------------------------###\n")
  mcmc$call <- paste0(mcmc$call, "N = sample size: ", N, "\n")
  mcmc$call <- paste0(mcmc$call, "n = number of units: ", n, "\n")
  mcmc$call <- paste0(mcmc$call, "observations per unit (min | median | max): ", 
                      min(nsubj), " | ", median(nsubj), " | ", max(nsubj), " \n")
  charG <- ifelse(length(unique(G)) == 1, 
                  G[1],
                  paste0(G, collapse = ", "))
  mcmc$call <- paste0(mcmc$call, "G = maximal number of components: ", charG, "\n")
  mcmc$call <- paste0(mcmc$call, "\nOverview of outcomes: \n")
  if(length(Nums) > 0){mcmc$call <- paste0(mcmc$call, "num: ", paste0(Nums, collapse = ", "), "\n")}
  if(length(Pois) > 0){mcmc$call <- paste0(mcmc$call, "poi: ", paste0(Pois, collapse = ", "), "\n")}
  if(length(Bins) > 0){mcmc$call <- paste0(mcmc$call, "bin: ", paste0(Bins, collapse = ", "), "\n")}
  if(length(Ords) > 0){mcmc$call <- paste0(mcmc$call, "ord: ", paste0(Ords, collapse = ", "), "\n")}
  if(length(Cats) > 0){mcmc$call <- paste0(mcmc$call, "cat: ", paste0(Cats, collapse = ", "), "\n")}
  mcmc$call <- paste0(mcmc$call, "\nFixed effects invariant towards clustering (in order of appearance): \n")
  for(y in Ys){
    if(length(lfixnames[[y]]) > 0){
      # add <- paste0(y, " ~ ", paste0(lfixnames[[y]], collapse = ", "))
      add <- paste0(y, " ~ ", as.character(formula[[y]]$fixed)[2])
      wadd <- strwrap(add, width = getOption("width"))
      for(l in 1:length(wadd)){
        mcmc$call <- paste0(mcmc$call, wadd[l], "\n")
      }
      # if(nchar(add) > getOption("width")){
      #   add <- substr(add, 1, getOption("width"))
      #   substr(add, getOption("width")-2, getOption("width")) <- "..."
      # }
      # mcmc$call <- paste0(mcmc$call, add, "\n")
    }else{
      mcmc$call <- paste0(mcmc$call, y, " ~ 0 \n")
    }
  }
  mcmc$call <- paste0(mcmc$call, "\nRandom effects invariant towards clustering: \n")
  for(y in Ys){
    add <- paste0(y, " ~ ", as.character(formula[[y]]$random)[2])
    wadd <- strwrap(add, width = getOption("width"))
    for(l in 1:length(wadd)){
      mcmc$call <- paste0(mcmc$call, wadd[l], "\n")
    }
    # if(nchar(add) > getOption("width")){
    #   add <- substr(add, 1, getOption("width"))
    #   substr(add, getOption("width")-2, getOption("width")) <- "..."
    # }
    # mcmc$call <- paste0(mcmc$call, add, "\n")
  }
  if(totnran > 0){
    mcmc$call <- paste0(mcmc$call, "\nThe structure of covariance matrix Sigma for random effects: \n")
    for(y in Ys){
      if(tuning$integer$kspec_bi_cat & (sorted_family[y] == "cat")){
        for(k in 1:Kcat[y]){
          for(l in 1:length(lrannames[[y]])){
            mcmc$call <- paste0(mcmc$call, y, "_", lrannames[[y]][l], ", k=", k, "\n")
          }
        }
      }else{
        for(l in 1:length(lrannames[[y]])){
          mcmc$call <- paste0(mcmc$call, y, "_", lrannames[[y]][l], "\n")
        }
      }
    }
  }else{
    mcmc$call <- paste0(mcmc$call, "\nThere are no random effects at all. 
All rows in the data are assumed to be independent as well as outcomes among themselves.")
  }
  mcmc$call <- paste0(mcmc$call, "\nGroup-specific effects for outcomes (in order of appearance): \n")
  for(y in Ys){
    if(length(lgrpnames[[y]]) > 0){
      # add <- paste0(y, " ~ ", paste0(lgrpnames[[y]], collapse = ", "))
      add <- paste0(y, " ~ ", as.character(formula[[y]]$group)[2])
      wadd <- strwrap(add, width = getOption("width"))
      for(l in 1:length(wadd)){
        mcmc$call <- paste0(mcmc$call, wadd[l], "\n")
      }
      # if(nchar(add) > getOption("width")){
      #   add <- substr(add, 1, getOption("width"))
      #   substr(add, getOption("width")-2, getOption("width")) <- "..."
      # }
      # mcmc$call <- paste0(mcmc$call, add, "\n")
    }else{
      mcmc$call <- paste0(mcmc$call, y, " ~ 0 \n")
    }
  }
  gparams <- rownames(settings)[as.logical(settings$gspec)]
  gparams <- gparams[!grepl("beta", gparams) & !is.element(gparams, c("w", "ng", "pUig"))]
  mcmc$call <- paste0(mcmc$call, "\nOther group-specific parameters: ", 
                      paste0(gparams, collapse = ", "), "\n")
  saved_params <- rownames(settings)[settings$save]
  # mcmc$call <- paste0(mcmc$call, "\nList of saved parameters in $draws: ", 
  #                     paste0(saved_params, collapse = ", "), "\n")
  mcmc$call <- paste0(mcmc$call, "\nMCMC sampling: \n")
  mcmc$call <- paste0(mcmc$call, "nchains = number of sampled chains: ", nchains, "\n")
  mcmc$call <- paste0(mcmc$call, "iter = length of the chain: ", iter, "\n")
  mcmc$call <- paste0(mcmc$call, ifelse(initsgiven, 
                                        "Initialized with given values.", 
                                        "Initialized by a random partition."),
                      "\n")
  

  mcmc$iter <- iter
  mcmc$iterations <- iterations
  mcmc$chains <- 1:nchains
  mcmc$nchains <- nchains
  mcmc$n <- n
  mcmc$numbered_unique_ids <- numbered_unique_ids
  mcmc$Nums <- Nums
  mcmc$Pois <- Pois
  mcmc$Bins <- Bins
  mcmc$Ords <- Ords
  mcmc$Cats <- Cats
  mcmc$family <- sorted_family
  mcmc$formula <- formula
  mcmc$G <- G
  mcmc$varying <- varying
  mcmc$save <- save
  mcmc$howsave <- howsave
  mcmc$param <- param
  mcmc$nfix <- nfix
  mcmc$ngrp <- ngrp
  mcmc$nran <- nran
  mcmc$noff <- noff
  mcmc$totnran <- totnran
  mcmc$lfixnames <- lfixnames
  mcmc$lgrpnames <- lgrpnames
  mcmc$lrannames <- lrannames
  mcmc$loffnames <- loffnames
  mcmc$fixnames <- fixnames
  mcmc$grpnames <- grpnames
  mcmc$rannames <- rannames
  mcmc$offnames <- offnames
  mcmc$nY <- nY
  mcmc$Kord <- Kord
  mcmc$Kcat <- Kcat
  mcmc$settings <- lsettings
  mcmc$tuning <- tuning
  mcmc$yspecd1 <- yspecd1
  mcmc$yspecd2 <- yspecd2
  mcmc$d2spec <- d2spec
  mcmc$isYna <- cisYna
  mcmc$standardize <- standardize
  mcmc$centers <- centers
  mcmc$scales <- scales
  # mcmc$post_processed <- FALSE
  
  class(mcmc) <- "clustglmm"
  return(mcmc)
  
}
