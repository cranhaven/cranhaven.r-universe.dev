# Mt model, capture probability time dependent

closedCapMt <-
function(CH, ci = 0.95, ciType=c("normal", "MARK"), ...) {
  # CH is a 1/0 capture history matrix, animals x occasions
  # ci is the required confidence interval

  crit <- fixCI(ci)
  ciType <- match.arg(ciType)

  CH <- round(as.matrix(CH))
  nocc <- ncol(CH)    # number of capture occasions
  N.cap <- nrow(CH)   # total number of individual animals captured
  n <- colSums(CH)    # vector of number captures on each occasion

  beta.mat <- matrix(NA_real_, nocc+1, 4) # objects to hold output
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c("Nhat", paste0("p", 1:nocc))
  npar <- NA_real_
  logLik <- NA_real_
  varcov <- NULL

  if(N.cap > 0)  {
    nll <- function(params) {
      N <- min(exp(params[1]) + N.cap, 1e+300, .Machine$double.xmax)
      logp <- plogis(params[-1], log.p=TRUE)
      log1mp <- plogis( -params[-1], log.p=TRUE)
      tmp <- lgamma(N + 1) - lgamma(N - N.cap + 1) +
        sum(n * logp + (N - n) * log1mp)
      return(min(-tmp, .Machine$double.xmax))
    }
    nlmArgs <- list(...)
    nlmArgs$f <- nll
    nlmArgs$p <- c(log(5), rep(0, nocc))
    nlmArgs$hessian <- TRUE
    if(is.null(nlmArgs$iterlim))
      nlmArgs$iterlim <- 1000
    res <- do.call(nlm, nlmArgs)
    if(res$code > 2)   # exit code 1 or 2 is ok.
      warning(paste("Convergence may not have been reached (nlm code", res$code, ")"))
    beta.mat[,1] <- res$estimate
    logLik <- -res$minimum
    varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
    if (!inherits(varcov0, "try-error")) {
      varcov <- varcov0
      beta.mat[, 2] <- suppressWarnings(sqrt(diag(varcov)))
      beta.mat[, 3:4] <- sweep(outer(beta.mat[, 2], crit), 1, res$estimate, "+")
      npar <- nocc+1
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
          real = rbind(Nhat, plogis(beta.mat[-1, -2])),
          logLik = c(logLik=logLik, df=npar, nobs=length(CH)))
  class(out) <- c("wiqid", "list")
  return(out)
}

