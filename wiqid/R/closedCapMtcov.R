# Mtcov model, capture probability a function of time dependent covariates

closedCapMtcov <-
function(CH, model=list(p~1), data=NULL, ci = 0.95, ciType=c("normal", "MARK"), ...) {
  # CH is a 1/0 capture history matrix, animals x occasions
  # ci is the required confidence interval

  crit <- fixCI(ci)
  ciType <- match.arg(ciType)

  # Standardise the model:
  model <- stdModel(model, list(p=~1))

  CH <- round(as.matrix(CH))
  nocc <- ncol(CH)    # number of capture occasions
  N.cap <- nrow(CH)   # total number of individual animals captured
  n <- colSums(CH)    # vector of number of captures on each occasion

  # Convert the covariate data frame into a list
  dataList <- stddata(data, nocc)
  dataList$.Time <- standardize(1:nocc)
  dataList$.Time2 <- dataList$.Time^2
  dataList$.Time3 <- dataList$.Time^3
  dataList$.time <- as.factor(1:nocc)
  ddf <- as.data.frame(dataList)

  pModMat <- modelMatrix(model$p, ddf)
  K <- ncol(pModMat)

  beta.mat <- matrix(NA_real_, K+1, 4) # objects to hold output
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c("Nhat", colnames(pModMat))
  lp.mat <- matrix(NA_real_, nocc, 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  rownames(lp.mat) <- paste0("p", 1:nocc)
  npar <- NA_real_
  logLik <- NA_real_
  varcov <- NULL

  if(N.cap > 0)  {
    nll <- function(params) {
      N <- min(exp(params[1]) + N.cap, 1e+300, .Machine$double.xmax)
      logitp <- pModMat %*% params[-1]
      logp <- as.vector(plogis(logitp, log.p = TRUE))
      log1mp <- as.vector(plogis( -logitp, log.p = TRUE))
      tmp <- lgamma(N + 1) - lgamma(N - N.cap + 1) +
        sum(n * logp + (N - n) * log1mp)
      return(min(-tmp, .Machine$double.xmax))
    }
    # res <- nlm(nll, params, hessian=TRUE, iterlim=1000)
    nlmArgs <- list(...)
    nlmArgs$f <- nll
    nlmArgs$p <- c(log(5), rep(0, K))
    nlmArgs$hessian <- TRUE
    if(is.null(nlmArgs$iterlim))
      nlmArgs$iterlim <- 1000
    res <- do.call(nlm, nlmArgs)
    if(res$code > 2)   # exit code 1 or 2 is ok.
      warning(paste("Convergence may not have been reached (code", res$code, ")"))
    beta.mat[,1] <- res$estimate
    lp.mat[, 1] <- pModMat %*% beta.mat[-1, 1]
    logLik <- -res$minimum
    varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
    if (!inherits(varcov0, "try-error")) {
      varcov <- varcov0
      beta.mat[, 2] <- suppressWarnings(sqrt(diag(varcov)))
      beta.mat[, 3:4] <- sweep(outer(beta.mat[, 2], crit), 1, res$estimate, "+")
      temp <- getFittedVar(pModMat, varcov[-1, -1])
      if(all(temp >= 0))  {
        SElp <- sqrt(temp)
        lp.mat[, 2:3] <- sweep(outer(SElp, crit), 1, lp.mat[, 1], "+")
        npar <- K+1
      }
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
          real = rbind(Nhat, plogis(lp.mat)),
          logLik = c(logLik=logLik, df=npar, nobs=length(CH)))
  class(out) <- c("wiqid", "list")
  return(out)
}
