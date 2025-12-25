#' @title Fitting a Cumulative Intensity Model for Exposure Effects with Instrumental Variables
#' @description ivsacim is used to fit cumulative intensity models for exposure effects with instrumental variables.
#' @param time the censored event time
#' @param event event indicator
#' @param instrument the instrumental variable
#' @param IV_valid whether assuming IV satisfies the exclusion restriction
#' @param treatment_init the initial treatment assignment
#' @param treatment_shift_time the shift time of each subject, if no shift for a subject, set as 0
#' @param max_time the max time that we threshold for nonconstant effect
#' @param max_time_bet the max time that we threshold for constant effect
#' @param n_sim the number of resampling, set as 0 if no resampling is needed
#' @param weights optional weights used in the estimating equation
#' @return ivsacim returns an object of class "ivsacim".
#' An object of class "ivsacim" is a list containing the following components:
#' \item{stime}{an estimate of the baseline hazards function}
#' \item{dB_D}{an estimate of the increment of the treatment effect}
#' \item{B_D}{an estimate of the treatment effect}
#' \item{beta_D}{an estimate of the constant treatment effect}
#' \item{B_D_se}{an estimate of the variance covariance matrix of B_D}
#' \item{beta_D_se}{an estimate of the constant treatment effect}
#' \item{by_prod}{a byproduct, that will used by other functions}
#' @importFrom stats rnorm
#' @export ivsacim
#' @examples
#' n = 400
#' event = rbinom(n, 1, 0.8)
#' IV = rbinom(n, 1, 0.5)
#' trt_init = IV
#' trt_shift = rep(0, n)
#' time = rexp(n)/(0.5 + trt_init * 0.2)
#' max_t = 3
#' max_t_bet = 3
#' n_sim = 0
#' fit <- ivsacim(time, event, IV, TRUE, trt_init, 
#' trt_shift, max_t, max_t_bet, n_sim)
ivsacim <- function (time, 
                     event, 
                     instrument,
                     IV_valid = TRUE,
                     treatment_init, 
                     treatment_shift_time = NULL, 
                     max_time = NULL, 
                     max_time_bet = NULL,
                     n_sim = 0,
                     weights = NULL) {
  ## generate needed arguments if not provided
  if (is.null(max_time)) 
    max_time = max(time)
  
  if (is.null(treatment_shift_time)) 
    treatment_shift_time = rep(0, length(time))
  
  if (is.null(weights))
    weights <- rep(1, length(time))
  
  event_new = event
  event_new[time > max_time] = 0
  stime = sort(time[event_new == 1])
  weights <- weights / mean(weights)
  
  ## IV_center is used to compute Z^c in the paper
  iv_centered <- IV_center(instrument, weights)
  Zc <- matrix(iv_centered$Zc)
  Z_model_mat <- matrix(iv_centered$Z_model_mat)
  eps_1 <- matrix(iv_centered$epstheta, nrow = 1)
  N <- length(time)
  K <- length(stime)
  D_status <- treatment_status(N = N, K = K, stime = stime, treatment_init = treatment_init, treatment_shift_time = treatment_shift_time, max_time = max_time) 

  if (IV_valid) {
    res <- ivsacim_est(time, 
                       event, 
                       stime, 
                       Zc, 
                       D_status, 
                       eps_1, 
                       Z_model_mat, 
                       weights)
  } 
  else {
    trt_centered <- trt_center(D_status, Z = instrument, weights)
    D_status_c <- trt_centered$D_status_c
    D_model_mat <- trt_centered$D_model_mat
    eps_2 <- trt_centered$epstheta
    res <- invalidivsacim_est(time, 
                              event, 
                              stime, 
                              instrument, 
                              Zc, 
                              D_status, 
                              D_status_c, 
                              Z_model_mat = Z_model_mat,
                              eps_1 = eps_1, 
                              D_model_mat = D_model_mat,
                              eps_2 = eps_2,
                              weights)
  }
  
  res$invalid_IV <- IV_valid
  B_D_IF = res$by_prod$B_D_IF
  k = length(stime < max_time_bet)
  beta_D_IF = res$by_prod$beta_D_IF
  
  if (n_sim > 0){
    GOF_resam0_D = matrix(0, nrow = n_sim, ncol = K)
    max_proc0 = numeric(n_sim)
    ## Const. effect
    GOF_resam_D = matrix(0, nrow = n_sim, ncol = K)
    max_proc = CvM_proc = numeric(n_sim)
    eps_const_eff = B_D_IF
    for(j in 1:K){
      eps_const_eff[j, ] = B_D_IF[j, ] - stime[j] * beta_D_IF
    }
    
    for(j1 in 1:n_sim){
      Q = rnorm(N, 0, 1)
      GOF_resam0_D[j1, ] = B_D_IF %*% Q
      max_proc0[j1] = max(abs(GOF_resam0_D[j1, ]))
      GOF_resam_D[j1, ] = eps_const_eff %*% Q
      max_proc[j1] = max(abs(GOF_resam_D[j1, ]))
    }
    GOF.proc = res$B_D[1:k] - res$beta_D * stime
    max_B_D_null = max(abs(res$B_D))
    max_B_D_const = max(abs(res$B_D - res$beta_D * stime))
    pval_D_null = sum(max_proc0 > max_B_D_null)/n_sim
    pval_D_const = sum(max_proc > max_B_D_const)/n_sim
    res$pval_D_null = pval_D_null
    res$pval_D_const = pval_D_const
    if (n_sim > 50){
      res$GOF_resamp_D = rbind(stime[1:K], GOF.proc, GOF_resam_D[1:50, ])
    }
    
    if(!IV_valid) {
      B_Z_IF = res$by_prod$B_Z_IF
      ## Const. effect
      GOF_resam = matrix(0, nrow = n_sim, ncol = K)
      max_proc = CvM_proc = numeric(n_sim)
      eps_const_eff = B_Z_IF
      for(j in 1:K){
        eps_const_eff[j, ] = B_D_IF[j, ] - stime[j] * beta_D_IF
      }
      
      GOF_resam0_Z = matrix(0, nrow = n_sim, ncol = K)
      GOF_resam_Z = matrix(0, nrow = n_sim, ncol = K)
      max_proc0 = numeric(n_sim)
      max_proc = numeric(n_sim)
      
      for(j1 in 1:n_sim){
        Q = rnorm(N, 0, 1)
        GOF_resam0_Z[j1, ] = B_Z_IF %*% Q
        max_proc0[j1] = max(abs(GOF_resam0_Z[j1, ]))
        GOF_resam_Z[j1, ] = eps_const_eff %*% Q
        max_proc[j1] = max(abs(GOF_resam_Z[j1, ]))
      }
      max_B_Z_null = max(abs(res$B_Z))
      max_B_Z_const = max(abs(res$B_Z - res$beta_Z * stime))
      pval_Z_null = sum(max_proc0 > max_B_Z_null)/n_sim
      pval_Z_const = sum(max_proc > max_B_Z_const)/n_sim
      res$pval_Z_null = pval_Z_null
      res$pval_Z_const = pval_Z_const
    }
  }
  res$by_prod$noresampling = (n_sim <= 0)
  res$by_prod$max_time = max_time
  res$by_prod$max_time_bet = max_time_bet
  res$by_prod$D_status = D_status
  res$by_prod$K = K
  class(res) <- "ivsacim"
  return(res)
  
}




#' @title Summarizing Cumulative Intensity Function of Treatment with Instrumental Variables 
#' Estimation Using Structural Additive Cumulative Intensity Models 
#' @param object an object of class "ivsacim", usually, a result of a call to ivsacim.
#' @param x an object of class "summary.ivsacim", usually, a result of a call to summary.ivsacim.
#' @param ... further arguments passed to or from other methods.
#' @description summary method for class "ivsacim".
#' @export ivsacim
#' @export summary.ivsacim
#' @importFrom stats pnorm pchisq
#' @method summary ivsacim
#' @S3method summary ivsacim
#' @examples
#' n = 400
#' event = rbinom(n, 1, 0.8) 
#' IV = rbinom(n, 1, 0.5)
#' trt_init = IV
#' trt_shift = rep(0, n)
#' time = rexp(n)/(0.5 + trt_init * 0.2)
#' max_t = 3
#' max_t_bet = 3
#' n_sim = 0
#' fit <- ivsacim(time, event, IV, IV_valid = TRUE, trt_init, trt_shift, max_t, max_t_bet, n_sim)
#' summary(fit)
#' @details print.summary.ivsacim tries to be smart about formatting coefficients, an estimated variance covariance matrix of
#' the coeffieients, Z-values and the corresponding P-values.
#' @return The function summary.ivsacim computes and returns a list of summary statistics of the fitted model given in object.
summary.ivsacim <- function(object, ...){
  obj <- object
  digits = 3
  chisq_beta_D = (obj$beta_D/obj$beta_D_se)^2
  pval_beta_D = 1 - pchisq(chisq_beta_D, df = 1)
  chisq_beta_Z = (obj$beta_Z/obj$beta_Z_se)^2
  pval_beta_Z = 1 - pchisq(chisq_beta_Z, df = 1)
  
  res <- list()
  res$pval_D_null = obj$pval_D_null
  res$beta_D = obj$beta_D
  res$beta_D_se = obj$beta_D_se
  res$pval_beta_D = pval_beta_D
  res$pval_D_const = obj$pval_D_const
  res$invalid_IV <- obj$invalid_IV
  if (!res$invalid_IV) {
    res$pval_Z_null = obj$pval_Z_null
    res$beta_Z = obj$beta_Z
    res$beta_Z_se = obj$beta_Z_se
    res$pval_beta_Z = pval_beta_Z
    res$pval_Z_const = obj$pval_Z_const
  }
  if (obj$by_prod$noresampling) {
    res$noresult = TRUE
  }
  else {
    res$noresult = FALSE
  }
  class(res) = "summary.ivsacim"
  res
}


#' @rdname summary.ivsacim
#' @S3method print summary.ivsacim
print.summary.ivsacim <- function(x, ...){
  obj <- x
  digits <- 3
  # We print information about object:  
  cat("   \n")
  cat("Instrumental Variables Structural Additive Cumulative Intensity Model")
  if (!obj$invalid_IV) {
    cat(" with an Invalid IV\n\n")
  } else {
    cat(" with a Valid IV\n\n")
  }
  if (!obj$noresult) { 
    test0 <- cbind(obj$pval_D_null)
    colnames(test0) <- c("Supremum-test pval")
    rownames(test0) <- c("Exposure")

    cat("Test for non-significant exposure effect. H_0: B_D(t) = 0 \n")
    cat("   \n")
    prmatrix(signif(test0, digits))
    cat("   \n")
    cat("\n")
    test_gof = cbind(obj$pval_D_const)
    colnames(test_gof) <- c("Supremum-test pval")
    rownames(test_gof) <- c("        ")
    cat("Goodness-of-fit test for constant effects model. H_0: B_D(t) = beta_D * t\n")
    prmatrix(signif(test_gof, digits))
    cat("   \n")
    
  }
  
  cat("Constant effect model: B_D(t) = beta_D * t \n")
  res <- cbind(obj$beta_D, obj$beta_D_se)
  z <- obj$beta_D/obj$beta_D_se
  pval <- obj$pval_beta_D
  res <- cbind(res, z, pval)
  rownames(res) <- c("Exposure     ")
  colnames(res) <- c("coef", "se(coef)", "z-value", "p-value")
  prmatrix(signif(res, digits))
  cat("   \n") 
  if (!obj$invalid_IV) {
    if (!obj$noresult) { 
      test0 <- cbind(obj$pval_Z_null)
      colnames(test0) <- c("Supremum-test pval")
      rownames(test0) <- c("Instrument")
      cat("Test for exclusion restriction. H_0: B_Z(t) = 0 \n")
      cat("   \n")
      prmatrix(signif(test0, digits))
      cat("   \n")
      cat("\n")
      test_gof = cbind(obj$pval_Z_const)
      colnames(test_gof) <- c("Supremum-test pval")
      rownames(test_gof) <- c("        ")
      cat("Goodness-of-fit test for constant effects model. H_0: B_Z(t) = beta_Z * t\n")
      prmatrix(signif(test_gof, digits))
      cat("   \n")
    }
    cat("Constant effect model: B_Z(t) = beta_Z * t \n")
    res <- cbind(obj$beta_Z, obj$beta_Z_se)
    z <- obj$beta_Z/obj$beta_Z_se
    pval <- obj$pval_beta_Z
    res <- cbind(res, z, pval)
    rownames(res) <- c("Instrument     ")
    colnames(res) <- c("coef", "se(coef)", "z-value", "p-value")
    prmatrix(signif(res, digits))
    cat("   \n") 
  }
  
  #cat("  Call: ")
  #dput(attr(obj, "Call"))
  #cat("\n")
  invisible()
}

#' @title Plotting Estimated Cumulative Intensity function with Pointwise Confidence Intervals
#' @param x the fitting object after fitting IVSACIM model
#' @param gof whether to draw the goodness-of-fit plot
#' @param ... the other arguments you want to put in the built-in plot function
#' @description The function will plot the estimated cumulative intensity function of the treatment after fitting.
#' Corresponding pointwise confidence intervals at level alpha are also included.
#' @export ivsacim
#' @export plot.ivsacim
#' @importFrom graphics plot lines abline par
#' @method plot ivsacim
#' @S3method plot ivsacim
#' @return No return value, called for side effects
#' @examples
#' n = 400
#' event = rbinom(n, 1, 0.8)
#' IV = rbinom(n, 1, 0.5)
#' trt_init = IV
#' trt_shift = rep(0, n)
#' time = rexp(n)/(0.5 + trt_init * 0.2)
#' max_t = 3
#' max_t_bet = 3
#' n_sim = 100
#' fit <- ivsacim(time, event, IV, IV_valid = TRUE, trt_init, trt_shift, max_t, max_t_bet, n_sim)
#' plot(fit, main = "", xlab = "Time", ylab = "Cumulative Intensity Function")
#' plot(fit, gof = TRUE, xlab = "Time", ylab = "")
plot.ivsacim <- function (x, gof = FALSE, ...){
  
  obj <- x
  if (!inherits(obj, 'ivsacim')) 
    stop ("Must be an ivsacim object")
  
  if(gof){
    #par(mfrow=c(1,1))
    minv = min(c(obj$GOF_resamp[2, ], obj$GOF_resamp[2:22, ]))
    maxv = max(c(obj$GOF_resamp[2, ], obj$GOF_resamp[2:22, ]))
    plot(obj$GOF_resamp[1, ], obj$GOF_resamp[2, ], type = "n", ylim = c(minv, maxv), ...)
    for(j in 1:20){
      lines(obj$GOF_resamp[1, ], obj$GOF_resamp[2 + j, ], type = "s", col = "grey")   
    }
    lines(obj$GOF_resamp[1, ], obj$GOF_resamp[2, ], type = "s", lty = 1, lwd = 3)
    abline(0, 0)	
    
  }else{
    if (obj$invalid_IV) {
      B_D <- obj$B_D
      B_D_se <- obj$B_D_se
      beta = obj$beta_D
      beta_se = obj$beta_D_se
      stime = obj$stime
      minv1 = min(c(B_D - 1.96 * B_D_se, beta * stime))
      maxv1 = max(c(B_D + 1.96 * B_D_se, beta * stime))
      
      #par(mfrow=c(1,1))
      plot(stime, B_D, type = "s", ylim = c(minv1, maxv1), ...)
      lines(stime, c(B_D + 1.96 * B_D_se), type = 's', lty = 2)
      lines(stime, c(B_D - 1.96 * B_D_se), type = 's', lty = 2)
      abline(0, 0)
      lines(stime, beta * stime)
      lines(stime, (beta + 1.96 * beta_se) * stime, lty = 2)
      lines(stime, (beta - 1.96 * beta_se) * stime, lty = 2)
    } else {
      par(mfrow=c(1,2))
      B_D <- obj$B_D
      B_D_se <- obj$B_D_se
      beta_D = obj$beta_D
      beta_D_se = obj$beta_D_se
      stime = obj$stime
      minv1 = min(c(B_D - 1.96 * B_D_se, beta_D * stime))
      maxv1 = max(c(B_D + 1.96 * B_D_se, beta_D * stime))
      
      #par(mfrow=c(1,1))
      plot(stime, B_D, type = "s", ylim = c(minv1, maxv1), ...)
      lines(stime, c(B_D + 1.96 * B_D_se), type = 's', lty = 2)
      lines(stime, c(B_D - 1.96 * B_D_se), type = 's', lty = 2)
      abline(0, 0)
      lines(stime, beta_D * stime)
      lines(stime, (beta_D + 1.96 * beta_D_se) * stime, lty = 2)
      lines(stime, (beta_D - 1.96 * beta_D_se) * stime, lty = 2)
      
      B_Z <- obj$B_Z
      B_Z_se <- obj$B_Z_se
      beta_Z = obj$beta_Z
      beta_Z_se = obj$beta_Z_se
      stime = obj$stime
      minv1 = min(c(B_Z - 1.96 * B_Z_se, beta_Z * stime))
      maxv1 = max(c(B_Z + 1.96 * B_Z_se, beta_Z * stime))
      
      #par(mfrow=c(1,1))
      plot(stime, B_Z, type = "s", ylim = c(minv1, maxv1), ...)
      lines(stime, c(B_Z + 1.96 * B_Z_se), type = 's', lty = 2)
      lines(stime, c(B_Z - 1.96 * B_Z_se), type = 's', lty = 2)
      abline(0, 0)
      lines(stime, beta_Z * stime)
      lines(stime, (beta_Z + 1.96 * beta_Z_se) * stime, lty = 2)
      lines(stime, (beta_Z - 1.96 * beta_Z_se) * stime, lty = 2)
    }
  }
  invisible()
}


