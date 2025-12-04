
# Maximum likelihood estimation for Cormack-Jolly-Seber apparent-survival models
#    of type phi(t) p(t) or with time covariates
# Added grouping factor
# Uses detection history matrix as input, not m-array.
# Uses multinomial likelihood.

# Grouping factor strategy:
#  Do m-array for each group, combine into a 3d array
#  Do design matrices with a set of occasions for each group
#  Calculate q-array and likelihood for each group and combine

log_qArray <- function(log_phi, log_p, log_1mp) {
  # Calculates the matrix of multinomial cell log(probabilities)
  #   corresponding to an m-array.
  # log_phi = vector of log(apparent survival probabilities)
  # log_p = vector of log(recapture probabilities)
  # log_1mp = vector of log(1 - recapture probabilities)
  # NO SANITY CHECKS, calling function must take care of this.

  n <- length(log_phi)

  # Create n x n+1 matrix and fill diagonal
  q <- diag(as.vector(log_p + log_phi), n, n+1)
  # Fill the upper triangle, and get the row sums
  sum_probs <- numeric(n)
  for (i in 1:(n-1)){
    for (j in (i+1):n) {
      q[i,j] <- sum(log_phi[i:j]) + sum(log_1mp[i:(j-1)]) + log_p[j]
    }
    sum_probs[i] <- logSumExp(q[i, i:n])
  }
  sum_probs[n] <- q[n, n]
  # Add the last column and return
  q[, n+1] <- log1minusExp(sum_probs)
  return(q)
}
# ..........................................................................

survCJS <- function(DH, model=list(phi~1, p~1), data=NULL, freq=1, group, interval=1,
  ci = 0.95, link=c("logit", "probit"), ...) {
  # ** DH is detection history matrix/data frame, animals x occasions.
  # ** freq is vector of frequencies for each detection history
  # ** model is a list of 2-sided formulae for psi and p; can also be a single
  #   2-sided formula, eg, model = psi ~ habitat.
  # ** data a data frame with the covariates.
  # ** group is a factor specifying which group each row of DH belongs to.
  # ** ci is required confidence interval.

  if(match.arg(link) == "logit") {
    plink <- plogis
  } else {
    plink <- pnorm
  }

  # Sanity checks:  for DH??
  ni <- ncol(DH) - 1  # number of survival intervals and REcapture occasions
  if(!is.null(data) && nrow(data) != ni)
    stop("The 'data' argument is not a valid data frame.")
  if(length(freq) == 1)
    freq <- rep(freq, nrow(DH))
  if (length(freq) != nrow(DH))
    stop("freq must have a value for each row of the detection history matrix.")

  if(!missing(group))  {
    if(length(group) != nrow(DH))
      stop("Group must have a value for each row of the detection history matrix.")
    group <- as.factor(group)
    nGroup <- nlevels(group)
    groupNames <- levels(group)
    data <- as.data.frame(cbind(data, group=rep(groupNames, each=ni)))
  } else {
    group <- NULL
    nGroup <- 1
  }
  if(length(interval) == 1)
    interval <- rep(interval, ni)
  if(length(interval) != ni)
    stop("'interval' must be scalar or length equal to number of intervals between captures")
  crit <- fixCI(ci)

  # Convert detection history to 3d array of m-arrays to facilitate use of multinomial likelihood
  mARRAY <- array(0, c(ni, ni+1, nGroup))
  if(nGroup == 1) {
    mARRAY[, , 1] <- ch2mArray(CH=DH, freq=freq)
  } else {
    for(i in 1:nGroup) {
      DHgrp <- subset(DH, group==groupNames[i])
      freqgrp <- subset(freq, group==groupNames[i])
      mARRAY[, , i] <- ch2mArray(CH=DHgrp, freq=freqgrp)
    }
  }

  # Standardise the model:
  model <- stdModel(model, defaultModel=list(phi=~1, p=~1))

  # Standardize the data
  dataList <- stddata(data, NULL)
  dataList$.Time <- standardize(1:ni)
  dataList$.Time2 <- dataList$.Time^2
  dataList$.Time3 <- dataList$.Time^3
  dataList$.time <- as.factor(1:ni)

  # Set up model matrices
  phiDf <- selectCovars(model$phi, dataList, ni*nGroup)
  phiMat <- modelMatrix(model$phi, phiDf)
  phiK <- ncol(phiMat)
  pDf <- selectCovars(model$p, dataList, ni*nGroup)
  pMat <- modelMatrix(model$p, pDf)
  pK <- ncol(pMat)
  K <- phiK + pK
  if(nrow(phiMat) != ni*nGroup || nrow(pMat) != ni*nGroup)
    stop("Missing values not allowed in covariates.")

  # Set up objects to hold output
  beta.mat <- matrix(NA_real_, K, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c(
    paste("phi:", colnames(phiMat)),
    paste("p:", colnames(pMat)))
  lp.mat <- matrix(NA_real_, ni*nGroup*2, 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  if(nGroup == 1) {
    rownames(lp.mat) <- c(
      paste0("phi", 1:ni),
      paste0("p", 1:ni))
  } else {
      rownames(lp.mat) <- c(
      paste0(data$group, ":phi", 1:ni),
      paste0(data$group, ":p", 1:ni))
  }
  npar <- NA_real_
  varcov <- NULL

  # Log likelihood function
  nll <- function(param){
    phiBeta <- param[1:phiK]
    pBeta <- param[(phiK+1):K]
    log_phi <- plink(phiMat %*% phiBeta, log.p=TRUE)
    link_p <- pMat %*% pBeta
    log_p <- plink(link_p, log.p=TRUE)
    log_1mp <- plink( -link_p, log.p=TRUE)
    if(nGroup == 1) {
      nll <-  -sum(mARRAY[, , 1] * log_qArray(log_phi*interval, log_p, log_1mp))
    } else {
      nll <- numeric(nGroup)
      for(i in 1:nGroup) {
        log_phi0 <- log_phi[data$group == groupNames[i]]
        log_p0 <- log_p[data$group == groupNames[i]]
        log_1mp0 <- log_1mp[data$group == groupNames[i]]
        nll[i] <- -sum(mARRAY[, , i] * log_qArray(log_phi0*interval, log_p0, log_1mp0))
      }
    }
    return(min(sum(nll), .Machine$double.xmax))
  }

  # Run mle estimation with nlm:
  # res <- nlm(nll, param, hessian=TRUE, stepmax=10) # 2015-03-01
  nlmArgs <- list(...)
  nlmArgs$f <- nll
  nlmArgs$p <- rep(0, K)
  nlmArgs$hessian <- TRUE
  if(is.null(nlmArgs$stepmax))
    nlmArgs$stepmax <- 10
  res <- do.call(nlm, nlmArgs)
  if(res$code > 2)   # exit code 1 or 2 is ok.
    warning(paste("Convergence may not have been reached (nlm code", res$code, ")"))

  # Process the output
  beta.mat[,1] <- res$estimate
  lp.mat[, 1] <- c(phiMat %*% beta.mat[1:phiK, 1],
                   pMat %*% beta.mat[(phiK+1):K, 1])
  logLik <- -res$minimum
  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  # if (!inherits(varcov0, "try-error") && all(diag(varcov0) > 0)) {
  if (!inherits(varcov0, "try-error")) {
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov)))
    beta.mat[, 2] <- SE
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, res$estimate, "+")
    SElp <- c(sqrt(getFittedVar(phiMat, varcov[1:phiK, 1:phiK])),
              sqrt(getFittedVar(pMat, varcov[(phiK+1):K, (phiK+1):K])))
    lp.mat[, 2:3] <- sweep(outer(SElp, crit), 1, lp.mat[, 1], "+")
    npar <- K
  }

  # Put it all together and return
  out <- list(call = match.call(),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plink(lp.mat),
              logLik = c(logLik=logLik, df=npar, nobs=sum(mARRAY)) )
  class(out) <- c("wiqid", "list")
  return(out)
}


