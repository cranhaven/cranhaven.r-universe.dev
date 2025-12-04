# Single season occupancy with no covariates (psi(.) p(.) model)

# 'link' argument added 2015-02-20

occSS0 <-
function(y, n, ci=0.95, link=c("logit", "probit"), ...) {
  # n is a vector with the number of occasions at each site.
  # y is a vector with the number of detections at each site.
  # ci is the required confidence interval.
  if(length(n) == 1)
    n <- rep(n, length(y))
  if(length(y) != length(n))
    stop("y and n must have the same length")
  if(any(y > n))
    stop("y cannot be greater than n")
  crit <- fixCI(ci)

  if(match.arg(link) == "logit") {
    plink <- plogis
  } else {
    plink <- pnorm
  }

  beta.mat <- matrix(NA_real_, 2, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c("psiHat", "pHat")
  logLik <- NA_real_
  npar <- NA_integer_
  varcov <- NULL

  if(sum(n) > 0) {    # If all n's are 0, no data available.
    nll <- function(params) {
       logpsi <- plink(params[1], log.p=TRUE)
       log1mpsi <- plink( -params[1], log.p=TRUE)
       logp <- plink(params[2], log.p=TRUE)
       log1mp <- plink( -params[2], log.p=TRUE)
       logprob <- logAddExp(logpsi + logp * y + log1mp * (n - y),
          log1mpsi + log(y==0))
      return(min(-sum(logprob), .Machine$double.xmax))
    }
    # res <- nlm(nll, params, hessian=TRUE)
    nlmArgs <- list(...)
    nlmArgs$f <- nll
    nlmArgs$p <- rep(0, 2)
    nlmArgs$hessian <- TRUE
    res <- do.call(nlm, nlmArgs)
    if(res$code > 2)   # exit code 1 or 2 is ok.
      warning(paste("Convergence may not have been reached (code", res$code, ")"))
    beta.mat[,1] <- res$estimate
    logLik <- -res$minimum

    varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
    if (!inherits(varcov0, "try-error")) {
      npar <- 2L
      varcov <- varcov0
      beta.mat[, 2] <- suppressWarnings(sqrt(diag(varcov)))
      beta.mat[, 3:4] <- sweep(outer(beta.mat[, 2], crit), 1, res$estimate, "+")
    }
  }
  out <- list(call = match.call(),
              link = match.arg(link),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plink(beta.mat[, -2]),
              logLik = c(logLik=logLik, df=npar, nobs=length(y)),
              ci = ci,
              formulae = list(psi= ~ 1, p = ~1),
              index = list(psi= 1, p = 2))
  class(out) <- c("wiqid", "list")
  return(out)
}
