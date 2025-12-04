closedCapM0 <-
function(CH, ci = 0.95, ciType=c("normal", "MARK"), ...) {
  # CH is a 1/0 capture history matrix, animals x occasions, OR
  #  a vector of capture frequencies of length equal to the number
  #  of occasions - trailing zeros are required.
  # ci is the required confidence interval
  # ciType is the method of calculation
  
  if (is.matrix(CH) || is.data.frame(CH)) {
    n.occ <- ncol(CH)
    freq <- tabulate(rowSums(CH), nbins=n.occ)
  } else {
    freq <- round(CH)
    n.occ <- length(freq)
  }
  crit <- fixCI(ci)
  ciType <- match.arg(ciType)

  N.cap <- sum(freq)  # Number of individual animals captured
  n.snap <- sum(freq * (1:length(freq))) # Total number of capture events
  beta.mat <- matrix(NA_real_, 2, 4) # objects to hold output
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c("Nhat", "phat")
  varcov <- NULL
  npar <- NA_real_
  logLik <- NA_real_
  if(sum(freq[-1]) > 0) {  # Need recaptures
    nll <- function(params) {
      N <- min(exp(params[1]) + N.cap, 1e+300, .Machine$double.xmax)
      logp <- plogis(params[2], log.p=TRUE)
      log1mp <- plogis( -params[2], log.p=TRUE)  # log(1-p)
      tmp <- lgamma(N + 1) - lgamma(N - N.cap + 1) + n.snap*logp +
            (N*n.occ - n.snap)*log1mp
      return(min(-tmp, .Machine$double.xmax))
    }
    nlmArgs <- list(...)
    nlmArgs$f <- nll
    nlmArgs$p <- c(log(5), 0)
    nlmArgs$hessian <- TRUE
    if(is.null(nlmArgs$iterlim))
      nlmArgs$iterlim <- 1000
    res <- do.call(nlm, nlmArgs)
    if(res$code > 2)   # exit code 1 or 2 is ok.
      warning(paste("Convergence may not have been reached (code", res$code, ")"))
    beta.mat[,1] <- res$estimate
    logLik <- -res$minimum
    varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
    if (!inherits(varcov0, "try-error")) {
      varcov <- varcov0
      beta.mat[, 2] <- suppressWarnings(sqrt(diag(varcov)))
      beta.mat[, 3:4] <- sweep(outer(beta.mat[, 2], crit), 1, res$estimate, "+")
      npar <- 2
    }
  }
  if(ciType == "normal") {
    Nhat <- exp(beta.mat[1, -2]) + N.cap
  } else {
    Nhat <- getMARKci(beta.mat[1, 1], beta.mat[1, 2], ci) + N.cap
  }

  out <- list(call = match.call(),
          beta = beta.mat,
          beta.vcv = varcov,
          real = rbind(Nhat, plogis(beta.mat[2, -2, drop=FALSE])),
          logLik = c(logLik=logLik, df=npar, nobs=N.cap * n.occ))
  class(out) <- c("wiqid", "list")
  return(out)
}
