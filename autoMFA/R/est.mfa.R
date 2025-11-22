est.mfa <- function (init_para, Y, itmax, tol, conv_measure, eta , ...)
{
  
  #This function is based on the function est.mfa from package EMMIXmfa.
  #See package EMMIXmfa for more details. 
  
  p <- ncol(Y)
  n <- nrow(Y)
  fit <- init_para
  loglikeNtau <- try(do.call("logL_tau.mfa", c(list(Y = Y),
                                               fit)), silent = TRUE)

  if ((any(class(loglikeNtau) %in% "try-error")) || (any(class(loglikeNtau) %in%
                                                         "character"))) {
    FIT <- paste("in computing the log-likelihood before EM-steps")
    class(FIT) <- "error"
    return(FIT)
  }
  fit <- append(fit, loglikeNtau)

  for (niter in 1:itmax) {


    FIT <- do.call("MStep.amfa", c(list(Y = Y, eta = eta), fit))
    if (any(class(FIT) %in% "error")) {
      FIT <- paste("in ", niter, "iteration of the M-step",
                   FIT)
      class(FIT) <- "error"
      return(FIT)
    }
    loglikeNtau <- try(do.call("logL_tau.mfa", c(list(Y = Y),
                                                 FIT)), silent = TRUE)
    if ((any(class(loglikeNtau) %in% "try-error")) || (any(class(loglikeNtau) %in%
                                                           "character"))) {
      FIT <- paste("in computing the log-likelihood after the ",
                   niter, "th the M-step", FIT$logL, sep = "")
      class(FIT) <- "error"
      return(FIT)
    }
    FIT <- append(FIT, loglikeNtau)
    if ((any(class(FIT$logL) == "NULL")) || (any(class(FIT$logL) ==
                                                 "character"))) {
      FIT <- paste("in computing the log-likelihood after the ",
                   niter, "th the M-step", FIT$logL, sep = "")
      class(FIT) <- "error"
      return(FIT)
    }
    else {
      if ((FIT$logL == -Inf) | is.na(FIT$logL)) {
        FIT <- paste("the log-likelihood computed after the ",
                     niter, "th iteration of the M-step is not finite",
                     sep = "")
        class(FIT) <- "error"
        return(FIT)
      }
    }
    if ((conv_measure == "diff") && (abs(FIT$logL - fit$logL) <
                                     tol))
      break
    if ((conv_measure == "ratio") && (abs((FIT$logL - fit$logL)/FIT$logL) <
                                      tol))
      break
    fit <- FIT
  }
  class(FIT) <- "mfa"
  return(FIT)
}
