
# New verion 2013-02-27 without the time argument (no time=FALSE option)

# 'link' argument added 2015-02-20
# 'verify' argument added 2016-09-20

occSStime <-
function(DH, model=p~1, data=NULL, ci=0.95,
    plot=TRUE, link=c("logit", "probit"), verify=TRUE, ...)  {
  # DH is a 1/0 matrix of detection histories, sites x occasions
  # model is a 2-sided formula for probability of detection, eg, model = p ~ habitat.
  # data is a DATA FRAME with a row for each capture occasion and columns for time covariates.
  # ci is the required confidence interval.

  # Sanity checks and such:
  DH <- as.matrix(DH)  # in case it's a data frame
  if(verify)
    DH <- verifyDH(DH, allowNA=TRUE)
	nocc <- ncol(DH)
  if (nocc < 2)
    stop("More than one survey occasion is needed")
  notDetected <- rowSums(DH, na.rm=TRUE) == 0 # TRUE if species NOT detected at the site
  if(!is.null(data) && nrow(data) != nocc)
    stop("'data' must have one row for each survey occasion.")
  crit <- fixCI(ci)

  if(match.arg(link) == "logit") {
    plink <- plogis
  } else {
    plink <- pnorm
  }

  # Standardise the model:
  model <- stdModel(model, defaultModel=list(p=~1))

  # Add built-in covars to the data frame
  dataList <- stddata(data, NULL, 0.5)
  dataList$.time <- as.factor(1:nocc)
  dataList$.Time <- standardize(1:nocc)
  dataList$.Time2 <- dataList$.Time^2
  dataList$.Time3 <- dataList$.Time^3
  pDf <- as.data.frame(dataList)

  # Do the model matrix for p:
  pModMat <- modelMatrix(model$p, pDf)
  pK <- ncol(pModMat)
  K <- pK + 1

  beta.mat <- matrix(NA_real_, K, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c("psi",
    paste("p:", colnames(pModMat)))
  lp.mat <- matrix(NA_real_, nocc + 1, 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  rownames(lp.mat) <- c("psi", paste0("p", 1:nocc))
  logLik <- NA_real_
  npar <- NA_integer_
  varcov <- NULL    # ????

  if(ncol(DH) > 1 && sum(DH, na.rm=TRUE) > 0)  {
    # Negative log-likelihood function:
    nll <- function(params) {
      logpsi <- plink(params[1], log.p=TRUE)
      log1mpsi <- plink( -params[1], log.p=TRUE)
      pBeta <- params[-1]
      linkp <- pModMat %*% pBeta
      logp <- plink(linkp, log.p=TRUE)
      log1mp <- plink( -linkp, log.p=TRUE)
      logLik1 <- sweep(DH, 2, logp, "*") + sweep((1-DH), 2, log1mp, "*")
      logLik2 <- rowSums(logLik1, na.rm=TRUE)
      llh <- sum(logAddExp(logpsi + logLik2, log1mpsi + log(notDetected)))
      return(min(-llh, .Machine$double.xmax)) # min(..) stops Inf being returned
    }

    nlmArgs <- list(...)
    nlmArgs$f <- nll
    nlmArgs$p <- rep(0, K)
    nlmArgs$hessian <- TRUE
    res <- do.call(nlm, nlmArgs)
    if(res$code > 2)   # exit code 1 or 2 is ok.
      warning(paste("Convergence may not have been reached (code", res$code, ")"))
    beta.mat[,1] <- res$estimate
    lp.mat[, 1] <- c(beta.mat[1], pModMat %*% beta.mat[-1,1])
    logLik <- -res$minimum

    varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
    # if (!inherits(varcov0, "try-error") && all(diag(varcov0) > 0)) {
    if (!inherits(varcov0, "try-error")) {
      npar <- K
      varcov <- varcov0
      SE <- suppressWarnings(sqrt(diag(varcov)))
      beta.mat[, 2] <- SE
      beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, res$estimate, "+")
      SElp <- c(sqrt(varcov[1,1]),
              sqrt(getFittedVar(pModMat, varcov[-1,-1] )))
      lp.mat[, 2:3] <- sweep(outer(SElp, crit), 1, lp.mat[, 1], "+")
    }

    # Do the plot
    if(plot) {
      real.p <- plink(lp.mat[-1, ])
      ylim <- range(0, real.p, na.rm=TRUE)
      plot(1:nocc, real.p[, 1], type='l', ylim=ylim,
        xlab="Time", ylab="Probability of detection")
      lines(1:nocc, real.p[, 2], lty=3)
      lines(1:nocc, real.p[, 3], lty=3)
    }
  }
  out <- list(call = match.call(),
              link=match.arg(link),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plink(lp.mat),
              logLik=c(logLik=logLik, df=npar, nobs=nrow(DH)))
  class(out) <- c("wiqid", "list")
  return(out)
}
