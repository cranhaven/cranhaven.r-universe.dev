# Single season occupancy with site covariates (not survey covariates)

# 'model' argument added 2013-12-02
# 'link' argument added 2015-02-20

occSScovSite <- function(y, n, model=NULL, data=NULL,
    ci=0.95, link=c("logit", "probit"), ...) {
  # single-season occupancy models with site-specific covatiates
  # new version with y/n input; much faster!
  # y is a vector with the number of detections at each site.
  # n is a vector with the number of occasions at each site.
  # model is a list of 2-sided formulae for psi and p; can also be a single
  #   2-sided formula, eg, model = psi ~ habitat.
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

  # Standardise the model:
  model <- stdModel(model, list(psi=~1, p=~1))

  # Convert the covariate data frame into a list
  nSites <- length(y)
  dataList <- stddata(data, nocc=NULL)

  psiDf <- selectCovars(model$psi, dataList, nSites)
  if (nrow(psiDf) != nSites)
    stop("Number of site covars doesn't match sites.")
  psiModMat <- modelMatrix(model$psi, psiDf)
  psiK <- ncol(psiModMat)
  pDf <- selectCovars(model$p, dataList, nSites)
  if (nrow(pDf) != nSites)
    stop("Number of site covars doesn't match sites.")
  pModMat <- modelMatrix(model$p, pDf)
  pK <- ncol(pModMat)
  K <- psiK + pK
  # modelMatrix removes rows with NAs:
  if(nrow(psiModMat) != nSites || nrow(pModMat) != nSites)
    stop("Missing site covariates are not allowed.")

  beta.mat <- matrix(NA_real_, K, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c(
    paste("psi:", colnames(psiModMat)),
    paste("p:", colnames(pModMat)))
  lp.mat <- matrix(NA_real_, nSites*2, 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  rownames(lp.mat) <- c(
    paste("psi:", 1:nSites, sep=""),
    paste("p:", 1:nSites, sep=""))
  logLik <- NA_real_
  npar <- NA_integer_
  varcov <- NULL

  nll <- function(param){
    psiBeta <- param[1:psiK]
    pBeta <- param[(psiK+1):K]
    logitpsi <- as.vector(psiModMat %*% psiBeta)
    logpsi <- plink(logitpsi, log.p=TRUE)
    log1mpsi <- plink( -logitpsi, log.p=TRUE)
    logitp <- as.vector(pModMat %*% pBeta)
    logp <- plink(logitp, log.p=TRUE)
    log1mp <- plink( -logitp, log.p=TRUE)
    logprob <- logAddExp(logpsi + logp * y + log1mp * (n - y),
        log1mpsi + log(y==0))
    return(min(-sum(logprob), .Machine$double.xmax))
  }

  # Run mle estimation with nlm:
  # res <- nlm(nll, param, hessian=TRUE)
  nlmArgs <- list(...)
  nlmArgs$f <- nll
  nlmArgs$p <- rep(0, K)
  nlmArgs$hessian <- TRUE
  res <- do.call(nlm, nlmArgs)
  if(res$code > 2)   # exit code 1 or 2 is ok.
    warning(paste("Convergence may not have been reached (nlm code", res$code, ")"))
  # Process output
  beta.mat[,1] <- res$estimate
  lp.mat[, 1] <- c(psiModMat %*% beta.mat[1:psiK, 1],
                   pModMat %*% beta.mat[(psiK+1):K, 1])
  logLik <- -res$minimum

  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  if (!inherits(varcov0, "try-error")) {
    npar <- K
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov)))
    beta.mat[, 2] <- SE
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, res$estimate, "+")
    temp <- c(getFittedVar(psiModMat, varcov[1:psiK, 1:psiK]),
              getFittedVar(pModMat, varcov[(psiK+1):K, (psiK+1):K]))
    if(all(temp >= 0))  {
      SElp <- sqrt(temp)
      lp.mat[, 2:3] <- sweep(outer(SElp, crit), 1, lp.mat[, 1], "+")
    }
  }
  out <- list(call = match.call(),
              link = match.arg(link),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plink(lp.mat),
              logLik = c(logLik=logLik, df=npar, nobs=length(y)))
  class(out) <- c("wiqid", "list")
  return(out)
}


