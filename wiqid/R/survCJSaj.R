# Estimation of apparent survival - CJS models

# These functions allow for Adult and Juvenile survival to differ,
#  eg. when birds are ringed as nestlings.

# Data should be organised as 2 detection history matrices, one for juveniles and one for
#   adults (when juveniles are recaptured they are already adults).

# qArrayAJ <- function(phi, p, phiJ=phi) {
log_qArrayAJ <- function(log_phi, log_p, log_1mp, log_phiJ=log_phi) {
  # Calculates the matrix of multinomial cell log(probabilities)
  #   corresponding to an m-array.
  # log_phi = vector of log(apparent survival probabilities)
  # log_phiJ = vector of log(apparent survival probabilities) for juveniles
  # log_p = vector of log(recapture probabilities)
  # log_1mp = vector of log(1 - recapture probabilities)
  # NO SANITY CHECKS, calling function must take care of this.

  n <- length(log_phi)

  # Create n x n+1 matrix and fill diagonal
  q <- diag(as.vector(log_p + log_phiJ), n, n+1)
  # Fill the upper triangle, and get the row sums
  sum_probs <- numeric(n)
  for (i in 1:(n-1)){
    for (j in (i+1):n) {
      q[i,j] <- log_phiJ[i] + sum(log_phi[(i+1):j]) + sum(log_1mp[i:(j-1)]) + log_p[j]
    }
    sum_probs[i] <- logSumExp(q[i, i:n])
  }
  sum_probs[n] <- q[n, n]
  # Add the last column and return
  q[, n+1] <- log1minusExp(sum_probs)
  return(q)
}
# ..........................................................................

survCJSaj <- function(DHj, DHa=NULL, model=list(phiJ~1, phiA~1, p~1), data=NULL,
    freqj=1, freqa=1, ci = 0.95, link=c("logit", "probit"), ...) {
  # phi(t) p(t) model or models with time covariates for Cormack-Joly-Seber
  # estimation of apparent survival.
  # ** DHj is detection history matrix/data frame, animals x occasions, for animals marked as juveniles; DHa (optional) has detection histories for animals marked as adults.
  # ** freqj and freqa are vectors of frequencies for each detection history
  # ** model is a list of 2-sided formulae for psiJ, psiA and p; can also be a single
  #   2-sided formula, eg, model = psiJ ~ habitat.
  # ** data a data frame with the covariates.
  # ** ci is required confidence interval.

    if(match.arg(link) == "logit") {
    plink <- plogis
  } else {
    plink <- pnorm
  }

  # Sanity checks:
  # Check DHj and DHa have same no. of columns ...
  nocc <- ncol(DHj)
  ni <- nocc - 1  # number of survival intervals and REcapture occasions
  if(!is.null(DHa) && ncol(DHa) != nocc)
    stop("'DHa' and 'DHj' must have the same number of columns.")
  if (length(freqj) == 1)
    freqj <- rep(freqj, nrow(DHj))
  # if (length(freqa) == 1)
    # freqa <- rep(freqa, nrow(DHa))  # Not needed

  if(ci > 1 | ci < 0.5)
    stop("ci must be between 0.5 and 1")
  alf <- (1 - ci[1]) / 2
  crit <- qnorm(c(alf, 1 - alf))

  # Deal with grownup juveniles, do m-array for these:
  grown <- DHj
  # Remove first capture
  getFirst <- function(x) min(which(x == 1))
  first <- apply(DHj, 1, getFirst)
  for(i in 1:nrow(grown))
    grown[i, first[i]] <- 0
  marrayA <- ch2mArray(grown, freqj)

  # Do m-array for juvenile juveniles
  ma <- matrix(0, nocc, nocc+1)
  for(i in 1:nrow(DHj)) {
    cht <- which(DHj[i, ] != 0) # When was animal caught?
    # Fill in release/recapture data
    # we are only interested in the first recapture
    if(length(cht) > 1)
      ma[cht[1], cht[2]] <- ma[cht[1], cht[2]] + freqj[i]
  }
  # Juveniles never seen again:
  ringed <- tapply(freqj, first, sum)
  ma[, nocc+1] <- c(ringed, 0) - rowSums(ma)
  marrayJ <- ma[-nocc, -1]

  # Add data for adults
  if(!is.null(DHa))
    marrayA <- marrayA + ch2mArray(DHa, freqa)

  # Standardise the model:
  model <- stdModel(model, defaultModel=list(phiJ=~1, phiA=~1, p=~1))

  # Standardize the data
  dataList <- stddata(data, NULL)
  dataList$.Time <- as.vector(scale(1:ni)) #/2
  dataList$.Time2 <- dataList$.Time^2
  dataList$.Time3 <- dataList$.Time^3
  dataList$.time <- as.factor(1:ni)

  # Set up model matrices
  phiADf <- selectCovars(model$phiA, dataList, ni)
  phiAMat <- modelMatrix(model$phiA, phiADf)
  phiAK <- ncol(phiAMat)
  phiJDf <- selectCovars(model$phiJ, dataList, ni)
  phiJMat <- modelMatrix(model$phiJ, phiJDf)
  phiJK <- ncol(phiJMat)
  pDf <- selectCovars(model$p, dataList, ni)
  pMat <- modelMatrix(model$p, pDf)
  pK <- ncol(pMat)
  K <- phiAK + phiJK + pK
  parID <- rep(1:3, c(phiAK, phiJK, pK))
  if(nrow(phiAMat) != ni || nrow(phiJMat) != ni || nrow(pMat) != ni)
    stop("Missing values not allowed in covariates.")

  # Objects to hold results
  beta.mat <- matrix(NA_real_, K, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c(
    paste("phiA:", colnames(phiAMat)),
    paste("phiJ:", colnames(phiJMat)),
    paste("p:", colnames(pMat)))
  lp.mat <- matrix(NA_real_, ni*3, 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  rownames(lp.mat) <- c(
    paste("phiA", 1:ni, sep=""),
    paste("phiJ", 1:ni, sep=""),
    paste("p", 1:ni, sep=""))
  npar <- NA_real_
  varcov <- NULL

  nll <- function(param){
    phiABeta <- param[parID==1]
    phiJBeta <- param[parID==2]
    pBeta <- param[parID==3]
    log_phiA <- plink(phiAMat %*% phiABeta, log.p=TRUE)
    log_phiJ <- plink(phiJMat %*% phiJBeta, log.p=TRUE)
    link_p <- pMat %*% pBeta
    log_p <- plink(link_p, log.p=TRUE)
    log_1mp <- plink( -link_p, log.p=TRUE)
    # Calculate the negative log(likelihood) value:
    return(min(-sum(marrayA  * log_qArrayAJ(log_phiA, log_p, log_1mp, log_phiA),   # adults
            marrayJ * log_qArrayAJ(log_phiA, log_p, log_1mp, log_phiJ)),   # juveniles
          .Machine$double.xmax))
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

  # Organise the output
  beta.mat[,1] <- res$estimate
  lp.mat[, 1] <- c(phiAMat %*% beta.mat[parID==1, 1],
                   phiJMat %*% beta.mat[parID==2, 1],
                   pMat %*% beta.mat[parID==3, 1])
  logLik <- -res$minimum
  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  if (!inherits(varcov0, "try-error")) {
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov)))
    beta.mat[, 2] <- SE
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, res$estimate, "+")
    temp <- sqrt(c(getFittedVar(phiAMat, varcov[parID==1, parID==1]),
             getFittedVar(phiJMat, varcov[parID==2, parID==2]),
             getFittedVar(pMat, varcov[parID==3, parID==3])))
    if(all(temp >= 0))  {
      SElp <- sqrt(temp)
      lp.mat[, 2:3] <- sweep(outer(SElp, crit), 1, lp.mat[, 1], "+")
    }
    npar <- K
  }
  out <- list(call = match.call(),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plink(lp.mat),
              logLik = c(logLik=logLik, df=npar, nobs=sum(marrayJ, marrayA)))
  class(out) <- c("wiqid", "list")
  return(out)
}


