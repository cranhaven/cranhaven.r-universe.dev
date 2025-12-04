
# Single-season 2-species occupancy function, based on Richmond et al 2010.

# Simplified analysis with no covariates.
# Called by occ2sps, not exported
# modPars is a vector of length 8 which maps the coefficients estimated to the
#   model parameters; order is
#   psiA, psiBA, psiBa, pA, pB, rA, rBa, rBA.

occ2sps0 <- function(DHA, DHB, modPars, ci=0.95)  {
  nSites <- nrow(DHA)
  crit <- fixCI(ci)

  # get the occupancy vector
  # psiX is a vector with elements psiA, psiBa, psiBA
  # getlogPHI <- function(psiX) {
    # log(c(psiX[1] * psiX[3],             # both
      # psiX[1] * (1 - psiX[3]),       # A only
      # (1 - psiX[1]) * psiX[2],       # B only
      # (1 - psiX[1]) * (1 - psiX[2]))) # neither
  # }

# Functions getlogPHI and getlogP moved to file occ2sps_utils.R 2017-10-30

  # objects to hold output
  beta.mat <- matrix(NA_real_, 8, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- names(modPars)
  logLik <- NA_real_
  npar <- NA_real_
  varcov <- NULL

  # Do the neg log lik function:
  nll <- function(params) {
    logitreal <- params[modPars] # this is a vector
    logPHI <- getlogPHI(t(logitreal[1:3])) # this is a vector
    logPHImat <- matrix(logPHI, nSites, 4, byrow=TRUE)
    logP <- getlogP(DHA, DHB, t(logitreal[4:8]))
    loglik <- apply(logPHImat + logP, 1, logSumExp)
    return(min(-sum(loglik), .Machine$double.xmax))
  }

  # Run mle estimation with optim:
  params <- rep(0, length(unique(modPars)))
  res <- optim(params, nll, method="L-BFGS-B", lower=-10, upper=10, hessian=TRUE)
  if(res$convergence > 0) {
    warning(paste("Convergence may not have been reached.", res$message))
  } else {
    logLik <- -res$value
  }

  beta.mat[,1] <- res$par[modPars]
  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  if (!inherits(varcov0, "try-error")) {
    npar <- length(unique(modPars))
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov))[modPars])
    beta.mat[, 2] <- SE
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, beta.mat[, 1], "+")
  }
  out <- list(call = match.call(),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plogis(beta.mat[, -2]),
              logLik = c(logLik=logLik, df=npar, nobs=nSites),
              ci = ci)
  class(out) <- c("wiqid", "list")
  return(out)
}
