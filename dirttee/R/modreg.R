#' @title 
#' Mode-regression for right-censored data
#' 
#' @description 
#' This function implements semiparametric kernel-based mode regression for right-censored or full data.
#' 
#' @export
#' @md
#' @importFrom mgcv gam model.matrix.gam
#' @importFrom formula.tools lhs
#' @importFrom survival is.Surv Surv survfit
#' @importFrom provenance KDE
#' @importFrom nloptr nloptr
#' @importFrom Matrix bdiag
#' @importFrom MASS ginv
#' @importFrom splines bs
#' @importFrom stats influence integrate
#' 
#' @param formula   A formula object, with the response on the left of the `~'
#' operator, and the terms on the right.  The response must be a
#' \code{Surv} object as returned by the \code{\link[survival]{Surv}} 
#' function. Only right censored data are allowed.
#' @param data A data set on which the regression should be performed on.
#' It should consist of columns that have the names of the specific variables
#' defined in \code{formula}. If \code{NULL}, the function will look for the
#' data in the environment given by the \code{formula} argument.
#' @param bw String, either "\code{Pseudo}", "\code{Plugin}" or a fixed numerical value. This determines how  
#' bandwidth should be estimated. "\code{Plugin}" only recommended for uncensored linear mode regression.
#' @param lambda Penalty term for penalized splines. Will be estimated if \code{NULL}.
#' @param KMweights numerical vector, should be the same length as the response. Inverse probability of censoring weights can be provided here. They will be calculated if \code{NULL}.
#' @param control A call to \code{\link{control}}. Various control parameters can be supplied here.
#' 
#' @details 
#' Fits mode regression in an iteratively weighted least squares approach. A detailed description of 
#' the approach and algorithm can be found in Seipp et al. (2022). In short, kernel-based mode regression leads
#' to minimization of weighted least squares, if the normal kernel is assumed. We use gam for estimation in each iteration. 
#' Mode regression is extended to right-censored time-to event data with inverse probability of censoring weights. 
#' Hyperparameters (bandwidth, penalty) are determined with a pseudo-likelihood approach for \code{bw = "Pseudo"}.
#' For "Plugin", plug-in bandwidth selection is performed, as described in Yao and Li (2014). However, this is only justified for uncensored data
#' and mode regression with linear covariate trends or known transformations. 
#' 
#' The event time has to be supplied using the \code{\link[survival]{Surv}} function. Positive event times with multiplicative relationships should be logarithmized 
#' beforehand. Nonlinear trends can be estimated with P-splines, indicated by using \code{\link[mgcv]{s}(covariate, bs = "ps")}. This will be passed down to gam, which is why 
#' the same notation is used. Other smooth terms are not tested yet. The whole gam object will be returned but standard errors and other information are not 
#' valid. \code{\link[dirttee]{boot.modreg}} can be used for calculation of standard errors and confidence intervals.
#'  
#' @references 
#' Seipp, A., Uslar, V., Weyhe, D., Timmer, A., & Otto-Sobotka, F. (2022). Flexible Semiparametric Mode Regression for Time-to-Event Data. Manuscript submitted for publication.\cr
#' Yao, W., & Li, L. (2014). A new regression model: modal linear regression. Scandinavian Journal of Statistics, 41(3), 656-671.
#' 
#' @examples 
#' 
#' \donttest{
#' data(colcancer)
#' colcancer80 <- colcancer[1:80, ]
#' 
#' # linear trend
#' regL <- modreg(Surv(logfollowup, death) ~ sex + age, data = colcancer80)
#' summary(regL)
#' 
#' # mode regression with P-splines. Convergence criteria are changed to speed up the function
#' reg <- modreg(Surv(logfollowup, death) ~ sex + s(age, bs = "ps"), data = colcancer80, 
#' control = modreg.control(tol_opt = 10^-2, tol_opt2 = 10^-2, tol = 10^-3))
#' summary(reg)
#' plot(reg)
#' 
#' # with a fixed penalty
#' reg2 <- modreg(Surv(logfollowup, death) ~ sex + s(age, bs = "ps"), data = colcancer80, lambda = 0.1)
#' 
#' # for linear effects and uncensored data, we can use the plug-in bandwidth
#' regP <- modreg(age ~ sex, data = colcancer, bw = "Plugin")
#' }
#'
#'
#' @returns
#' This function returns a list with the following properties:
#' \item{reg}{object of class \link[mgcv]{gam}. Should be interpreted with care.}
#' \item{bw}{The used bandwidth.}
#' \item{converged}{logical. Whether or not the iteratively weighted least squares algorithm converged.}
#' \item{iterations}{the number of iterations of the final weighted least squares fit}
#' \item{cova}{Covariance matrix. Only supplied in case of linear terms and plug-in bandwidth.}
#' \item{KMweights}{double vector. Weights used.}
#' \item{called}{list. The arguments that were provided.}
#' \item{aic}{Pseudo AIC.}
#' \item{pseudologlik}{Pseudo log-likelihood.}
#' \item{edf}{Effective degrees of freedom}
#' \item{delta}{vector. Indicating whether an event has occured (1) or not (0) in the input data.}
#' \item{response}{vector with response values}
#' \item{hp_opt}{Summary of hyperparameter estimation.}


modreg <-
  function(formula, data = NULL, bw = c("Pseudo", "Plugin"), lambda = NULL, KMweights = NULL, control = NULL){
    
    called <- match.call()
    
    # control
    
    # StartInterval <- sqrt(3)
    # nStart <- 11
    # nInterim <- NULL
    # maxit <- 100
    # itInterim <- 10
    # tol <- 10^-4
    # tol_bw_plugin <- 10^-3
    # maxit_bw_plugin <- 10
    # maxit_penalty_plugin <- 10
    # tol_penalty_plugin <- 10^-3
    # tol_regopt <- tol * 100
    # tol_opt <- 10^-3
    # maxit_opt <- 200
    # tol_opt2 <- 10^-3
    # maxit_opt2 <- 200
    
    if(is.null(control)){
      control <- modreg.control()
    }else if(!inherits(control,"modreg_control")) stop("The argument control must be a call of the control function.")
    
    StartInterval        <- control$StartInterval
    nStart               <- control$nStart
    nInterim             <- control$nInterim
    maxit                <- control$maxit
    itInterim            <- control$itInterim
    tol                  <- control$tol
    tol_bw_plugin        <- control$tol_bw_plugin
    maxit_bw_plugin      <- control$maxit_bw_plugin
    maxit_penalty_plugin <- control$maxit_penalty_plugin
    tol_penalty_plugin   <- control$tol_penalty_plugin
    tol_regopt           <- control$tol_regopt
    tol_opt              <- control$tol_opt
    maxit_opt            <- control$maxit_opt
    tol_opt2             <- control$tol_opt2
    maxit_opt2           <- control$maxit_opt2
    
    
    #e <- environment()
    #sapply(names(control),function(x)assign(x, control[[x]], e))
    
    
    #if(!is.null(control) && !is.list(control)) stop("control has to be a list")
    #for(ctr in seq_along(control)){
    #  assign(names(control)[ctr], control[[ctr]])
    #}
    
    #########################################################!!! Veraendert
    fdat <- prepare_formula(formula, data)
    response <- fdat[[1]]
    delta    <- fdat[[2]]
    data     <- fdat[[3]]
    formula_gam <- fdat[[4]]
    if(!all(complete.cases(data))) stop("Missing data is not allowed")
    
    # if(any(!colnames(data) %in% get.vars(formula)))stop('Please make sure that all the Variables of the formula are found in the data.')
    if(!is.null(lambda) & !is.numeric(lambda))stop('lambda must be numeric.')
    
    
    #formula.tools::lhs(formula_gam) <- eval(parse(text = paste0('quote(',attr(response,"name"),')')))
    #response <- as.numeric(response)
    ##############################################################################
 
   
    if(is.null(KMweights)){
      if(identical(delta, rep(1, length(response)))){
        data$KMweights <- rep(1, length(response))
      } else{
        data$KMweights <- weightsKM(response, delta)$weightsIPC
      } 
    }else{
      #if(!all(cc)) stop("KMweights are not allowed with missing data.")
      data$KMweights <- KMweights
    }
    
    

    
    if(!is.numeric(bw)){
      bw <- match.arg(bw)
    }
    
    called[["bw"]] <-bw
    
    if((nStart %% 2) == 0) nStart <- nStart + 1
    if(is.null(nInterim)){
      if(nStart <= 5){
        nInterim <- nStart
      } else{
        nInterim <- pmax(0.1 * nStart, 5)
      }
    } else{
      if(nInterim < nStart) stop("nInterim must not be smaller than nStart")
    }
    
    if(itInterim >= maxit) stop("itInterim must be smaller than maxit.")
    

    reg <- mgcv::gam(formula_gam, data = data, weights = KMweights) #########!!!
    
    #response <- reg$y
    design <- mgcv::model.matrix.gam(reg)
    p <- sum(influence(reg))
    
    n <- length(response)
    
    ns <- summary(reg)$m
    
    lambda_fixed <- FALSE
    if(ns >= 1){
      if(is.null(lambda)){
        lambda <- rep(1, ns)
      } else{
        lambda_fixed <- TRUE
      }
    }
    
    #if(ns >= 1){
    #  lambda <- reg$sp
    #}
    
    
    
    ######################################################################    
    if(bw == "Plugin"){
      
      ctr <- 1
      counter <- 1
      predi <- NULL
      repeat{
        
        #
        # inner start
        #
        
        repeat{
          
          if(is.null(predi)){
            sigma <- sqrt(reg$sig2)  # weights?
            predLM <- predict(reg)
            ceV <- seq(-StartInterval, StartInterval, length.out = nStart)
          } else{
            predLM <- predi
            ceV <- sigma <- 0
          }
        
          h <- bwidth_plugin(response - predLM, X = design)
          if(counter != 1 && (counter >= maxit_bw_plugin || abs(h_old - h) < tol_bw_plugin)){
            break
          }
          h_old <- h
          
          
          
          score_interim <- numeric()
          reg_interim <- list()
          for(i in seq_along(ceV)){
            
            pred <- predLM + ceV[i] * sigma
            residuals <- response - pred
            
            itera <- regIt(formula = formula_gam, data = data, delta = delta, h = h, maxit = itInterim, tol = tol, residuals = residuals, pred = pred, response = response, lambda = lambda, ns = ns, p = p)
            
            score_interim[i] <- itera$score
            reg_interim[[i]] <- itera$reg
          }
          
          if(sum(!is.na(score_interim)) < nInterim){
            nbest <- sum(!is.na(score_interim))
          }else{
            nbest <- nInterim
          }
          
          best_interim <- order(score_interim, decreasing = TRUE, na.last = TRUE)[1:nbest]
          
          p <- sum(influence(reg_interim[[best_interim[1]]]))
          
          best_score <- NA
          for(i in best_interim){
            
            pred <- predict(reg_interim[[i]])
            residuals <- response - pred
            
            itera <- regIt(formula = formula_gam, data = data, delta = delta, h = h, maxit = maxit - itInterim, tol = tol, residuals = residuals, pred = pred, response = response, lambda = lambda, ns = ns, p = p)
            
            
            if(is.na(best_score) || itera$score > best_score){
              best_score <- itera$score
              best <- itera
            }
          }
          
          
          predi <- predict(best$reg)
          
          if(counter == 1){
            if(is.numeric(bw)) {
              break
            }
            nInterim <- 1
          }
          
          
          counter <- counter + 1
          
        }
        
        
        #
        # inner stop
        #
        
        
        if(is.na(best_score)) return(best = list(reg = NA, bw = NA, score = NA,
                                                 hp_opt = NA, cova = NA))
        
        
        if(ns == 0 || ctr >= maxit_penalty_plugin || lambda_fixed){
          break
        }
        
        lambda_old <- lambda
        
        #data$w1 <- data$KMweights * exp(-((response - predict(best$reg)) ^ 2) / (2 * h ^ 2))
        #reg_gcv <- gam(formula, data = data, weights = w1, method = smooth.method)
        #lambda <- reg_gcv$sp

        opt2 <- nloptr::nloptr(x0 = lambda_old,
                               eval_f = smooth_eval,
                               lb = rep(0, length(lambda_old)),
                               ub = pmax(rep(5, length(lambda_old)), 10 * lambda_old),
                               opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                           "xtol_rel" = tol_opt2,
                                           "maxeval" = maxit_opt2
                               ),
                               formula = formula_gam, 
                               data = data, 
                               delta = delta, 
                               maxit = pmin(maxit, 20),
                               tol = tol_regopt, 
                               residuals = residuals, 
                               pred = predi, 
                               response = response, 
                               ns = ns,
                               p = p,
                               target = "lambda",
                               h = h, 
                               lambda = NULL
        )
        
        lambda <- opt2$solution
        
        
        
        if(all(abs(lambda - lambda_old) < tol_penalty_plugin)){
          break
        }
        
        ctr <- ctr + 1
        counter <- 1
      }
      
      
      
      ######################################################################    
    } else{
      
      
      sigma <- sqrt(reg$sig2)  # weights?
      predLM <- predict(reg)
      ceV <- seq(-StartInterval, StartInterval, length.out = nStart)
      
      
      residuals <- response - predLM
      
      if(bw == "Pseudo"){
        roder <- order(abs(residuals))
        bwr <- sort(abs(residuals)) / 3
        kmr <- data$KMweights[roder]
        bwr1 <- bwr[kmr != 0]
        kmr1 <- kmr[kmr != 0]
        
        q01_init <- min(bwr1)
        q09_init <- max(bwr1)
        
        mbwr_init <- bwr1[which.max(cumsum(kmr1) >= 0.5 * sum(kmr1))]
        mbwr_init <- max(mbwr_init, q01_init)
        mbwr_init <- min(mbwr_init, q09_init)
        
        if(ns == 0 || lambda_fixed){
          
       
          opt <- nloptr::nloptr(x0 = mbwr_init,
                                eval_f = smooth_eval,
                                lb = q01_init,
                                ub = q09_init,
                                opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                            "xtol_rel" = tol_opt,
                                            "maxeval" = maxit_opt
                                ),
                                formula = formula_gam, 
                                data = data, 
                                delta = delta, 
                                maxit = maxit,
                                tol = tol_regopt, 
                                residuals = residuals, 
                                pred = predLM, 
                                response = response, 
                                ns = ns,
                                p = p,
                                target = "h",
                                h = NULL,
                                lambda = lambda
          )
        } else{
          opt <- nloptr::nloptr(x0 = c(mbwr_init, lambda),
                                eval_f = smooth_eval,
                                lb = c(q01_init, rep(0, length(lambda))),
                                ub = c(q09_init, rep(Inf, length(lambda))),
                                opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                            "xtol_rel" = tol_opt,
                                            "maxeval" = maxit_opt
                                ),
                                formula = formula_gam, 
                                data = data, 
                                delta = delta, 
                                maxit = maxit,
                                tol = tol_regopt, 
                                residuals = residuals, 
                                pred = predLM, 
                                response = response, 
                                ns = ns,
                                p = p,
                                target = "both",
                                h = NULL,
                                lambda = NULL
          )
          lambda <- opt$solution[-1]
        }
        #if(opt$objective == 10^10 || opt$iterations == maxit_opt) warning("Hyperparameter selection did not converge")
        h <- opt$solution[1]
      }
      
      if(is.numeric(bw)){
        h <- bw
        
        if(ns != 0 && !lambda_fixed){
          opt <- nloptr::nloptr(x0 = lambda,
                                eval_f = smooth_eval,
                                lb = rep(0, length(lambda)),
                                ub = rep(Inf, length(lambda)),
                                opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                            "xtol_rel" = tol_opt,
                                            "maxeval" = maxit_opt
                                ),
                                formula = formula_gam, 
                                data = data, 
                                delta = delta, 
                                maxit = maxit,
                                tol = tol_regopt, 
                                residuals = residuals, 
                                pred = predLM, 
                                response = response, 
                                ns = ns,
                                p = p,
                                target = "lambda",
                                h = h,
                                lambda = NULL
          )
          lambda <- opt$solution
          #if(opt$objective == 10^10 || opt$iterations == maxit_opt) warning("Hyperparameter selection did not converge")
        }
      }
      
      score_interim <- numeric()
      reg_interim <- list()
      for(i in seq_along(ceV)){
        
        pred <- predLM + ceV[i] * sigma
        residuals <- response - pred
        
        itera <- regIt(formula = formula_gam, data = data, delta = delta, h = h, maxit = itInterim, tol = tol, residuals = residuals, pred = pred, response = response, lambda = lambda, ns = ns, p = p)
        
        score_interim[i] <- itera$score
        reg_interim[[i]] <- itera$reg
      }
      
      if(sum(!is.na(score_interim)) < nInterim){
        nbest <- sum(!is.na(score_interim))
      }else{
        nbest <- nInterim
      }
      
      best_interim <- order(score_interim, decreasing = TRUE, na.last = TRUE)[1:nbest]
      if(is.na(score_interim[best_interim[1]])) return(best = list(reg = NA, bw = NA, score = NA, converged = NA, cova = NA))
      
      p <- sum(influence(reg_interim[[best_interim[1]]]))
      
      best_score <- NA
      for(i in best_interim){
        
        pred <- predict(reg_interim[[i]])
        residuals <- response - pred
        itera <- regIt(formula = formula_gam, data = data, delta = delta, h = h, maxit = maxit - itInterim, tol = tol, residuals = residuals, pred = pred, response = response, lambda = lambda, ns = ns, p = p)
        
        
        if(is.na(best_score) || itera$score > best_score){
          best_score <- itera$score
          best <- itera
        }
      }
      
      
      if(is.na(best_score)) return(best = list(reg = NA, bw = NA, score = NA,
                                               converged = NA, cova = NA))
      
      lambda_old <- lambda
      predi <- predict(best$reg)
      residuals <- response - predi
      
      if(bw == "Pseudo"){
        roder <- order(abs(residuals))
        bwr <- sort(abs(residuals)) / 3
        kmr <- data$KMweights[roder]
        bwr1 <- bwr[kmr != 0]
        kmr1 <- kmr[kmr != 0]
        q01_init <- min(bwr1)
        q09_init <- max(bwr1)
        
        mbwr_init <- h
        mbwr_init <- max(mbwr_init, q01_init)
        mbwr_init <- min(mbwr_init, q09_init)
        
        if(ns == 0 || lambda_fixed){
          opt2 <- nloptr::nloptr(x0 = h,
                                 eval_f = smooth_eval,
                                 lb = q01_init,
                                 ub = q09_init,
                                 opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                             "xtol_rel" = tol_opt2,
                                             "maxeval" = maxit_opt2
                                 ),
                                 formula = formula_gam, 
                                 data = data, 
                                 delta = delta, 
                                 maxit = maxit,
                                 tol = tol_regopt, 
                                 residuals = residuals, 
                                 pred = predi, 
                                 response = response, 
                                 ns = ns,
                                 p = p,
                                 target = "h",
                                 h = NULL,
                                 lambda = lambda_old
          )
          if(opt2$objective == 10^10 || opt$iterations == maxit_opt2){
            warning("Hyperparameter selection did not converge")
          } 
        } else{
          opt2 <- nloptr::nloptr(x0 = c(h, lambda_old),
                                 eval_f = smooth_eval,
                                 lb = c(q01_init, rep(0, length(lambda_old))),
                                 ub = c(q09_init, rep(Inf, length(lambda_old))),
                                 opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                             "xtol_rel" = tol_opt2,
                                             "maxeval" = maxit_opt2
                                 ),
                                 formula = formula_gam, 
                                 data = data, 
                                 delta = delta, 
                                 maxit = maxit,
                                 tol = tol_regopt, 
                                 residuals = residuals, 
                                 pred = predi, 
                                 response = response, 
                                 ns = ns,
                                 p = p,
                                 target = "both",
                                 h = NULL,
                                 lambda = NULL
          )
          if(opt2$objective == 10^10 || opt2$iterations == maxit_opt2){
            warning("Hyperparameter selection did not converge")
          } 
          lambda <- opt2$solution[-1]
        }
        
        h <- opt2$solution[1]
        
        
        
      } else{
        if(ns != 0 && !lambda_fixed){
          opt2 <- nloptr::nloptr(x0 = lambda_old,
                                 eval_f = smooth_eval,
                                 lb = rep(0, length(lambda_old)),
                                 ub = rep(Inf, length(lambda_old)),
                                 opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                             "xtol_rel" = tol_opt2,
                                             "maxeval" = maxit_opt2
                                 ),
                                 formula = formula_gam, 
                                 data = data, 
                                 delta = delta, 
                                 maxit = maxit,
                                 tol = tol_regopt, 
                                 residuals = residuals, 
                                 pred = predi, 
                                 response = response, 
                                 ns = ns,
                                 p = p,
                                 target = "lambda",
                                 h = h,
                                 lambda = NULL
          )
          if(opt2$objective == 10^10 || opt2$iterations == maxit_opt2){
            warning("Hyperparameter selection did not converge")
          } 
          lambda <- opt2$solution
        }
      }
      
      if(bw == "Pseudo" || (ns != 0 & !lambda_fixed)){
        best <- regIt(formula = formula_gam, data = data, delta = delta, h = h, maxit = maxit, tol = tol, residuals = residuals, pred = pred, response = response, lambda = lambda, ns = ns, p = p)
        if(is.na(best$score)) return(best = list(reg = NA, bw = NA, score = NA, converged = NA, cova = NA))
      }
      
    
      #best$hp_itera <- opt2$iterations
      
    }
    
    # Covarianzmatrix (nur plugin)
    if(ns == 0 & identical(delta, rep(1, length(response)))){
      residuals <- response - predict(best$reg)
      n <- length(residuals)
      
      # dens <- KDE(residuals_trimmed, from = min(residuals_trimmed), to = max(residuals_trimmed), adaptive = FALSE, n = 10000)
      dens <- provenance::KDE(residuals, from = min(residuals), to = max(residuals), adaptive = FALSE, n = 10000)
      
      bw_inner <- dens$bw
      
      m <- dens$x[which.max(dens$y)]
      
      g0 <- 1 / (n * bw_inner) * sum(dnorm((residuals - m) / bw_inner))
      L <- 1 / n * g0 * t(design) %*% design
      
      g2 <- 1 / (n * bw_inner ^ 3) * sum(norm2((residuals - m) / bw_inner))
      J <- 1 / n * g2 * t(design) %*% design
      
      v2 <- integrate(function(x) x ^ 2 * dnorm(x) ^ 2, -Inf, Inf)$value
      
      best$cova <- v2 * solve(J) %*% L %*% solve(J) / (n * best$bw ^ 3)
      
      
    }
    
    best$KMweights <- data$KMweights
    
    called_list <- list()
    n0 <- TRUE
    for(m in 2:length(called)){
      called_list[[m-1]] <- eval(called[[m]], envir = parent.frame(n = 1))
      n0[m] <- is.null(eval(called[[m]], envir = parent.frame(n = 1)))
    }
    #names(called_list) <- names(called)[-1]
    
    best$called <- called_list

    names(best$called) <- names(called)[!n0]
    
    best$lambda <- lambda
    
    # aic
    plscore <-   pl(bw0 = best$bw, bwreg = best$reg, response = response, 
                    dat = data, delta = delta, lambda = lambda, ns = ns)
    
    best$aic <- ifelse(plscore == -10^10, NA, - 2 * plscore + 2 * sum(influence(best$reg)))
    
    #plscore = pseudologlik
    #sum(influence(best$reg))) = edf

    ######################################################GEaeNDERT###############
    best <- best[!names(best) %in% c('score','score_raw')]
    best$pseudologlik <- plscore
    best$edf <- sum(influence(best$reg))
    best$delta <- delta
    best$response <- response
    if(exists("opt2")){
      best$hp_opt <- opt2
    } else{
      best$hp_opt <- list(status =FALSE, iterations = FALSE)
    }
    if(called$bw == "Plugin"){
      best$iterations_plugin <- counter
    }
    class(best) <- 'modreg'
    ##############################################################################
    ####################################geaendert
    return(best)
  }