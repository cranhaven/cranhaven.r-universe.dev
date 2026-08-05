clustering_probabilities_and_deviance <-
function(mcmc,      # MCMC generated parameters
         #   output of Metropolis_within_Gibbs_MBC_NumBinOrdCat
         id,     # name of the id variable in data
         data,   # outcomes, regressors and ids of subjects
         dynamic_prob = FALSE, # Should probabilities be calculated dynamically?
         #dev = TRUE,
         start = 1,
         end = mcmc$iter,
         thin = 1,
         chains = mcmc$chains,
         howsave = mcmc$howsave,
         tolerance = mcmc$tuning$double$tolerance, # tolerance when computing norm of the shift within Newton-Raphson
         maxiter = mcmc$tuning$integer$maxiter, # maximum iterations allowed during Newton-Raphson update of proposal distribution
         maxnrep = mcmc$tuning$integer$maxnrep,
         NGQ = 1 # number of points for Adaptive Gaussian Quadrature
         # default value 1 corresponds to Laplacian approximation
         # do not use values higher than 7, since there are NGQ^totnran summands
){
  ### Every auxiliary values are stored within mcmc output
  # Such as save, calc, varying, ...
  # message("\nR function pUig_dev succesfully called.\n")
  if(!inherits(mcmc, "clustglmm")){
    stop("mcmc is not 'clustglmm' object")
  }
  
  # Find roots and weights for Adaptive Gaussian Quadrature
  # require("gaussquad")
  # require("polynom")
  # require("orthopolynom")
  if(NGQ == 1){
    rules <- hermite.he.quadrature.rules(n = 2, normalized=FALSE)[[1]]
  }else{
    rules <- hermite.he.quadrature.rules(n = NGQ, normalized=FALSE)[[NGQ]]
  }
  
  # For probability calculation we need the following parameters:
  #   w, prec_num, beta, InvSigma
  # If those were not saved during the clustGLMM function,
  # then we cannot use this function to calculate it.
  if(
     !(mcmc$save["w"] & mcmc$save["c_ord"] & mcmc$save["prec_num"] 
       & mcmc$save["beta_num_fix"] & mcmc$save["beta_num"] 
       & mcmc$save["beta_poi_fix"] & mcmc$save["beta_poi"] 
       & mcmc$save["beta_bin_fix"] & mcmc$save["beta_bin"] 
       & mcmc$save["beta_ord_fix"] & mcmc$save["beta_ord"] 
       & mcmc$save["beta_cat_fix"] & mcmc$save["beta_cat"])
     ){
    stop("Some of the parameters w, c_ord, prec_num, betas were not saved.
Change the settings and use clustGLMM() again.")
  }
  
  if((!mcmc$save["InvSigma"]) & (mcmc$settings[[chains[1]]]["InvSigma", "dims"] > 0)){
    stop("Parameter InvSigma was not saved and its dimension is > 0 (i.e., it exists).
Change the settings and use clustGLMM() again.")
  }
  
  # Calculation will be done by C function, to speed up calculation
  # parameters need to be given to C function in a similar way to Metropolis_within_Gibbs_MBC_NumBinOrdCat
  
  N <- dim(data)[1]
  unique_ids <- unique(data[,id])
  n <- length(unique_ids)
  numbered_unique_ids <- c(1:n)
  names(numbered_unique_ids) <- unique_ids
  nsubj <- table(data[,id])
  max_n_i <- max(nsubj)
  
  Ys <- c(mcmc$Nums, mcmc$Pois, mcmc$Bins, mcmc$Ords, mcmc$Cats)
  nY <- sapply(list(mcmc$Nums, mcmc$Pois, mcmc$Bins, mcmc$Ords, mcmc$Cats), length)
  names(nY) <- c("Nums", "Pois", "Bins", "Ords", "Cats")
  
  ydata <- data
  # Numeric
  if(nY["Nums"] > 0){
    for(y in mcmc$Nums){
      yval <- data[,y]
      if(!is.numeric(yval)){
        stop(paste0("Outcome ", y, " is not numeric!"))
      }
      if(sum(infs <- is.infinite(yval))>0){
        yval[infs] <- NA
        warning(paste0("Infinite values of outcome ", y, " have been converted to NAs."))
      }
      ydata[,y] <- yval # numeric values
    }
  }
  
  # Poisson
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){abs(x - as.integer(x)) < tol}
  if(nY["Pois"] > 0){
    for(y in mcmc$Pois){
      yval <- data[,y]
      # if(any(is.infinite(yval))){
      #  stop(paste0("There are infinite values for outcome ", y, "!"))
      #}
      if(sum(infs <- is.infinite(yval))>0){
        yval[infs] <- NA
        warning(paste0("Infinite values of outcome ", y, " have been converted to NAs."))
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
    for(y in mcmc$Bins){
      if(is.factor(data[,y])){
        fy <- data[,y]
      }else{
        fy <- factor(data[,y])
      }
      if(nlevels(fy) != 2){
        stop(paste0("Binary outcome ", y, " does not have 2 levels."))
      }
      ydata[,y] <- as.numeric(fy)-1 # 0, 1 values
    }
  }
  
  # Ordinal
  if(nY["Ords"] > 0){
    for(y in mcmc$Ords){
      if(is.factor(data[,y])){
        fy <- data[,y]
      }else{
        fy <- factor(data[,y])
      }
      if(!is.ordered(fy)){
        fy <- factor(fy, ordered = TRUE)
        warning(paste0("Converting ordinal outcome ", y, " to ordered factor with levels: ",
                       paste(levels(fy), collapse = " < ")))
      }
      if(nlevels(fy) <= 2){
        stop(paste0("Ordinal outcome ", y, " has less than 3 levels."))
      }
      ydata[,y] <- as.numeric(fy)-1 # 0 < 1 < ... < Kord[y]
    }
  }
  
  # Categorical
  if(nY["Cats"] > 0){
    for(y in mcmc$Cats){
      if(is.factor(data[,y])){
        fy <- data[,y]
      }else{
        fy <- factor(data[,y])
      }
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
    }
  }
  
  
  # Creating model matrix containing all needed columns for C
  finX <- data.frame(id = data[,id])
  Xcolnames <- numeric()
  fcols <- gcols <- rcols <- ocols <- list()
  for(y in Ys){
    fauxX <- model.matrix(mcmc$formula[[y]]$fixed, data)
    gauxX <- model.matrix(mcmc$formula[[y]]$group, data)
    rauxX <- model.matrix(mcmc$formula[[y]]$random, data)
    if(is.element(y, mcmc$Ords)){
      fcols[[y]] <- colnames(fauxX)[-1]
      gcols[[y]] <- colnames(gauxX)[-1]
    }else{
      fcols[[y]] <- colnames(fauxX)
      gcols[[y]] <- colnames(gauxX)
    }
    fcols[[y]] <- setdiff(fcols[[y]], gcols[[y]])
    rcols[[y]] <- colnames(rauxX)
    
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
    
    ocols[[y]] <- mcmc$formula[[y]]$offset
    noff <- ifelse(mcmc$formula[[y]]$offset == "", 0, 1)
    if(noff > 0){
      if(mcmc$family[y] == "num"){
        if(is.element(mcmc$formula[[y]]$offset, colnames(data))){
          ydata[,y] <- ydata[,y] - data[,mcmc$formula[[y]]$offset]
        }else{
          warning(paste0("Data.frame data does not contain an offset ", mcmc$formula[[y]]$offset, " for variable ", y, ".
Working under zero offset."))
        }
      }else{
        noto <- setdiff(ocols[[y]], Xcolnames)
        Xcolnames <- c(Xcolnames, noto)
        addX <- data.frame(data[,noto])
        colnames(addX) <- noto
        finX <- cbind(finX, addX)
      }
    }
    
  }
  
  params <- c("beta_num_fix", "beta_num", "prec_num", 
              "beta_poi_fix", "beta_poi", 
              "beta_bin_fix", "beta_bin", 
              "beta_ord_fix", "beta_ord", "c_ord", 
              "beta_cat_fix", "beta_cat", 
              "InvSigma", 
              "w", "pUig_int", "deviance", "dev_i")
  ydepparams <- c(paste0("beta_", c("num", "poi", "bin", "ord", "cat"), "_fix"),
                  paste0("beta_", c("num", "poi", "bin", "ord", "cat")),
                  "c_ord")
  d2spec <- list()
  if(dynamic_prob){
    d2spec[["pUig_int"]] <- nsubj
  }
  # otherwise empty
  
  ### Preparation of parameters for Gibbs sampler in C
  ## Data
  # First column is going to be the id variable (0-th column)
  cId <- numbered_unique_ids[as.character(data[,id])] - 1
  # -1 is there for C which works better with number beginning with 0
  # other columns  (beginning with 1st column)
  cY <- numeric()
  for(y in Ys){
    if(is.element(y, mcmc$Cats)){
      # Change the first category to be the last, otherwise preserve the order
      auxy <- ydata[,y]
      auxy[auxy==0] <- mcmc$Kcat[y]+1
      auxy <- auxy - 1
      cY <- c(cY, auxy)
    }else{
      cY <- c(cY, ydata[,y])
    }
    #cY <- c(cY, as.numeric(as.character(data[,y])))
  }
  cisYna <- is.na(cY)
  cY[cisYna] <- 0 # NA is not sent to C function
  
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
    cFormulaF <- c(cFormulaF, Xcolnums[mcmc$lfixnames[[y]]])
    cFormulaG <- c(cFormulaG, Xcolnums[mcmc$lgrpnames[[y]]])
    cFormulaR <- c(cFormulaR, Xcolnums[mcmc$lrannames[[y]]])
    cFormulaO <- c(cFormulaO, Xcolnums[mcmc$loffnames[[y]]])
  }
  cFormulaF <- cFormulaF - 1 # index in C
  cFormulaG <- cFormulaG - 1 # index in C
  cFormulaR <- cFormulaR - 1 # index in C
  cFormulaO <- cFormulaO - 1 # index in C
  cnfix <- as.numeric(mcmc$nfix) # number of FIXED  regressors with variables y
  cngrp <- as.numeric(mcmc$ngrp) # number of GROUP-SPECIFIC  regressors with variables y
  cnran <- as.numeric(mcmc$nran) # number of RANDOM regressors with variables y
  cnoff <- as.numeric(mcmc$noff) # number of OFFSET regressors with variables y
  
  # head(finX)
  # all the following three are going to be saved
  # Iterations to be taken
  iters <- list()
  niter <- numeric(mcmc$nchains)
  nparams <- numeric(mcmc$nchains)
  output <- cmcmc <- cnames <- param_names <- list()
  lsettings <- list()
  for(ch in chains){
    # iterations
    ms <- mcmc$draws[[ch]]$m
    ms <- ms[(ms >= start) & (ms <= end)]
    # only those that do not cover NA values created by post_processing()
    iters[[ch]] <- intersect(ms[seq(1, length(ms), by=thin)],
                             mcmc$iterations[[ch]])
    niter[ch] <- length(iters[[ch]])
    # settings
    settings <- mcmc$settings[[ch]]
    rownames(settings)[rownames(settings)=="loglik"] <- "deviance"
    rownames(settings)[rownames(settings)=="pUig"] <- "pUig_int"
    settings["deviance", "save"] <- TRUE
    settings["pUig_int", "save"] <- TRUE
    if(dynamic_prob){
      settings["pUig_int", c("d2spec", "BYROW")] <- c(TRUE, TRUE)
      settings["pUig_int", c("d1", "d2", "D", "dims", "dimswithG")] = c(n, max_n_i, 2, N, N*mcmc$G[ch])
    }else{
      settings["pUig_int",  c("d1", "dims", "dimswithG")] = c(n, n, n*mcmc$G[ch])
    }
    settings_devi <- settings["U", ]
    settings_devi[,"save"] <- TRUE
    settings_devi[,c("d1", "dims", "dimswithG")] <- c(n, n, n)
    rownames(settings_devi) <- "dev_i"
    settings <- rbind(settings, settings_devi)
    
    settings <- settings[params,]
    settings$iter <- niter[ch]
    lsettings[[ch]] <- settings
    subsettings <- settings[c("deviance", "dev_i", "pUig_int"), ]
    
    nparams[ch] <- sum(subsettings$dimswithG * subsettings$save)
    param_names[[ch]] <- list()
    cnames[[ch]] <- character(1+nparams[ch])
    cnames[[ch]][1] = "m"
    index = 1
    for(p in c("deviance", "dev_i", "pUig_int")){
      if(settings[p, "save"]){
        aux = from_C_to_matrix(values = rep(NA, settings[p,"dimswithG"]),
                               p = p, settings = settings, 
                               d2spec = d2spec[[p]], family = mcmc$family)
        param_names[[ch]][[p]] <- colnames(aux)
        cnames[[ch]][(index+1):(index+settings[p,"dimswithG"])] = colnames(aux)
        index = index + settings[p,"dimswithG"]
      }
    }
  }
  
  
  
  for(ch in chains){
    cparamvalues <- list()
    # arrays/fields where those variables are and will be stored
    cparamvalues[["pUig_int"]] <- double(niter[ch]*lsettings[[ch]]["pUig_int", "dimswithG"])
    cparamvalues[["deviance"]] <- double(niter[ch]*lsettings[[ch]]["deviance", "dimswithG"])
    cparamvalues[["dev_i"]] <- double(niter[ch]*lsettings[[ch]]["dev_i", "dimswithG"])
    #if(dev){cdeviance <- double(niter*cdimswithK["deviance"])}else{cdeviance <- as.double(0)}
    
    if(mcmc$howsave == "list"){
      for(p in params[1:14]){
        cparamvalues[[p]] <- from_list_to_C(mcmc$draws[[ch]], 
                                            iters[[ch]],
                                            p, 
                                            lsettings[[ch]], 
                                            yspecd1=mcmc$yspecd1[[p]],
                                            yspecd2=mcmc$yspecd2[[p]],
                                            family=mcmc$family)
      }
    }
    
    if(mcmc$howsave == "data.frame"){
      for(p in params[1:14]){
        cparamvalues[[p]] <- from_matrix_to_C(mcmc$draws[[ch]], iters[[ch]], p, lsettings[[ch]])
      }
    }
    
    if(mcmc$howsave == "cmcmc"){
      stop("howsave = \"cmcmc\" is not supported.")
    }
    
    #dyn.load("Cfun/pUig_dev.dll")
    #dyn.unload("Cfun/pUig_dev.dll")
    
    # print("About to start C computations.")
    # print(ch)
    # print(mcmc$G)
    # print(niter)
    # print(N)
    # print(n)
    # print(mcmc$nY)
    # print(cFormulaF)
    # print(cFormulaG)
    # print(cFormulaR)
    # print(cFormulaO)
    # print(cnfix)
    # print(cngrp)
    # print(cnran)
    # print(cnoff)
    # print(mcmc$totnran)
    # print(mcmc$Kord)
    # print(mcmc$Kcat)
    # print(lsettings[[ch]]$dims)
    # print(lsettings[[ch]]$dimswithG)
    # print(mcmc$tuning$integer$kspec_bi_cat)
    # print(cbeta_num_fix)
    # print(cbeta_num)
    # print(ctau_num)
    # print(cbeta_poi_fix)
    # print(cbeta_poi)
    # print(cbeta_bin_fix)
    # print(cbeta_bin)
    # print(cbeta_ord_fix)
    # print(cbeta_ord)
    # print(cc_ord)
    # print(cbeta_cat_fix)
    # print(cbeta_cat)
    # print(cInvSigma)
    # print(cw)
    # print(cpUig_int)
    # print(cdeviance)
    # print(cdev_i)
    # print(dynamic_prob)
    # print(NGQ)
    # print(rules$x)
    # print(rules$w)
    # print(tolerance)
    # print(maxiter)
    # print(maxnrep)
    
    #system.time(
    cmcmc[[ch]] <-
      .C(C_pUig_dev,
         Id        = as.integer(cId),
         Y         = as.double(cY),
         isYna     = as.integer(cisYna),
         X         = as.double(cX),
         varying   = as.integer(mcmc$varying),
         # parameters describing dimensions
         chain     = as.integer(ch), # number of the chain
         G         = as.integer(mcmc$G), # number of classes
         iter      = as.integer(niter[ch]), # total number of generated states
         N         = as.integer(N), # total number of observations
         n         = as.integer(n), # total number of subjects (different ids in the dataset)
         nY        = as.integer(mcmc$nY), # 3 numbers: counts of Nums, Ords and Bins variables
         FormulaF  = as.integer(cFormulaF), # numbers of columns of X that should be used for FIXED  effects of modelled responses
         FormulaG  = as.integer(cFormulaG), # numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
         FormulaR  = as.integer(cFormulaR), # numbers of columns of X that should be used for RANDOM effects of modelled responses
         FormulaO  = as.integer(cFormulaO), # numbers of columns of X that should be used for OFFSET effects of modelled responses
         nfix      = as.integer(cnfix), 
         ngrp      = as.integer(cngrp), 
         nran      = as.integer(cnran),
         noff      = as.integer(cnoff),
         totnran   = as.integer(mcmc$totnran),
         Kord      = as.integer(mcmc$Kord), # the counts of categories of ordinal variables (-1)
         Kcat      = as.integer(mcmc$Kcat), # the counts of categories of categorical variables (-1)
         dims      = as.integer(lsettings[[ch]]$dims), # the length of subarray that corresponds to one state (disected by various parameters)
         dimswithG = as.integer(lsettings[[ch]]$dimswithG), # the length of subarray that corresponds to one state
         kspec_bi_cat = as.integer(mcmc$tuning$integer$kspec_bi_cat),
         #predictor = double(N*sum(nY)),
         # the function should count totnran, totnfix and cumsum versions of nfix and nran
         # arrays to store generated states
         beta_num_fix = as.double(cparamvalues[["beta_num_fix"]]),
         beta_num     = as.double(cparamvalues[["beta_num"]]),
         tau_num      = as.double(cparamvalues[["prec_num"]]),
         beta_poi_fix = as.double(cparamvalues[["beta_poi_fix"]]),
         beta_poi     = as.double(cparamvalues[["beta_poi"]]),
         beta_bin_fix = as.double(cparamvalues[["beta_bin_fix"]]),
         beta_bin     = as.double(cparamvalues[["beta_bin"]]),
         beta_ord_fix = as.double(cparamvalues[["beta_ord_fix"]]),
         beta_ord     = as.double(cparamvalues[["beta_ord"]]),
         c_ord        = as.double(cparamvalues[["c_ord"]]),
         beta_cat_fix = as.double(cparamvalues[["beta_cat_fix"]]),
         beta_cat     = as.double(cparamvalues[["beta_cat"]]),
         InvSigma     = as.double(cparamvalues[["InvSigma"]]),
         w            = as.double(cparamvalues[["w"]]),
         pUig_int     = as.double(cparamvalues[["pUig_int"]]),
         deviance     = as.double(cparamvalues[["deviance"]]),
         dev_i        = as.double(cparamvalues[["dev_i"]]),
         dynamic_prob = as.integer(dynamic_prob),
         NGQ          = as.integer(NGQ),
         roots        = as.double(rules$x),
         weights      = as.double(rules$w),
         tolerance    = as.double(tolerance),
         maxiter      = as.integer(maxiter),
         maxnrep      = as.integer(maxnrep)
      )
    
    if(howsave != "data.frame"){
      # results will be returned as it was returned by C function
      chain <- list()
      for(p in c("deviance", "dev_i", "pUig_int")){
        if(lsettings[[ch]][p, "save"]){
          
          if(howsave == "cmcmc"){
            chain[[p]] <- cmcmc[[ch]][[p]]
          }
          
          if(howsave == "list"){
            chain[[p]] <- from_C_to_list(cmcmc[[ch]][[p]], p, lsettings[[ch]], 
                                         d2spec = d2spec[[p]], family=mcmc$family)
          }
        }
      }
      chain[["m"]] <- iters[[ch]]
      
      output[[ch]] <- chain
    }

    if(howsave == "data.frame"){
      # results will be returned in matrix - the same way as original R function
      output[[ch]] <- matrix(NA, nrow = niter[ch], ncol = 1+nparams[ch])
      output[[ch]][,1] <- iters[[ch]]
      colnames(output[[ch]]) <- cnames[[ch]]
      output[[ch]] <- as.data.frame(output[[ch]])
      
      for(p in c("deviance", "dev_i", "pUig_int")){
        if(lsettings[[ch]][p, "save"]){
          aux = from_C_to_matrix(cmcmc[[ch]][[p]], p, lsettings[[ch]], 
                                 d2spec = d2spec[[p]], family=mcmc$family)
          output[[ch]][, colnames(aux)] <- aux
        }
      }
    }
    
    lsettings[[ch]] <- lsettings[[ch]][c("deviance", "dev_i", "pUig_int"), ]
    
  }  # end of for ch in 1:chains
  
  # create a final output object
  res <- mcmc
  res$draws <- output
  res$chains <- chains
  res$settings <- lsettings
  res$param_names <- param_names
  res$n <- n
  res$N <- N
  res$numbered_unique_ids <- numbered_unique_ids
  res$d2spec <- d2spec
  res$howsave <- howsave
  
  # perform clustering based on approximated values
  clusters <- list()
  modeGplus <- numeric(mcmc$nchains)
  clustering <- matrix(NA, nrow = as.numeric(n), ncol = mcmc$nchains)
  certainty <- matrix(NA, nrow = as.numeric(n), ncol = mcmc$nchains)
  for(ch in chains){
    probs <- from_C_to_matrix(cmcmc[[ch]][["pUig_int"]], "pUig_int", lsettings[[ch]], 
                              d2spec=d2spec[["pUig_int"]], family=mcmc$family)
    if(dynamic_prob){
      # keep only the clustering probabilities for all available observations
      pnames <- paste0("pUig_int(", rep(1:mcmc$G[ch], each=n), 
                       ")[", rep(1:n, mcmc$G[ch]), ",", 
                       rep(nsubj, mcmc$G[ch]), "]")
      probs <- probs[,pnames]
    }
    psum <- apply(probs, 2, mean, na.rm=TRUE)
    probmean <- matrix(psum, ncol = mcmc$G[ch], nrow = n, byrow = FALSE)
    
    clustering[,ch] <- apply(probmean, 1, which.max)
    certainty[,ch] <- apply(probmean, 1, max)
    clusters[[ch]] <- sort(unique(clustering[,ch]))
    modeGplus[ch] <- length(clusters[[ch]])
  }
  rownames(clustering) <- rownames(certainty) <- unique_ids
  # TODO remain unclustered with label 0
  
  # res$modeGplus <- modeGplus # keep it the same or make it the number of non-empty clusters after classification?
  # res$clusters <- clusters
  res$clustering <- clustering
  res$certainty <- certainty
  
  sameclusters <- TRUE
  for(ch in chains[-1]){
    sameclusters <- (sameclusters & setequal(mcmc$clusters[[chains[1]]], mcmc$clusters[[ch]]))
  }
  res$sameclusters <- sameclusters
  
  # call
  call <- c("###-------------------------------------------------------------------------------###\n")
  call <- paste0(call, "### Approximation of posterior distribution of clustering probabilities and deviance ###\n")
  call <- paste0(call, "###-------------------------------------------------------------------------------###\n")
  call <- paste0(call, "N = sample size: ", N, "\n")
  call <- paste0(call, "n = number of units: ", n, "\n")
  call <- paste0(call, "observations per unit (min | median | max): ", 
                 min(nsubj), " | ", median(nsubj), " | ", max(nsubj), " \n")
  call <- paste0(call, "G = maximal number of components: ", paste(mcmc$G, collapse=", "), "\n")
  
  saved_params <- rownames(res$settings[[chains[1]]])[res$settings[[chains[1]]]$save]
  call <- paste0(call, "\nList of saved parameters in $draws: ", 
                 paste0(saved_params, collapse = ", "), "\n")
  call <- paste0(call, "\nMCMC approximation: \n")
  call <- paste0(call, "start = first iteration number used: ", start, "\n")
  call <- paste0(call, "end = last iteration number used: ", end, "\n")
  call <- paste0(call, "thin = thinning applied to iterations between start and end: ", thin, "\n")
  call <- paste0(call, "chains = chains used: ", paste(chains, collapse=", "), "\n")
  call <- paste0(call, "niter = number of iterations used for approximation (per chain): ", 
                 paste(niter, collapse = ", "), "\n")
  call <- paste0(call, "dynamic_prob = ", as.character(dynamic_prob), " = ", 
                 ifelse(dynamic_prob,
                        "dynamically calculated probabilities", 
                        "probabilities calculated from all available observations"), 
                 "\n")
  if(mcmc$totnran > 0){
    call <- paste0(call, "Integral wrt random effects approximated with NGQ=", NGQ, "\n")
  }

  res$call <- call
  
  
  class(res) <- "clustglmm"
  
  return(res)
  
}
