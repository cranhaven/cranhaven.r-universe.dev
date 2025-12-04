
# Multiseason occupancy

# See MacKenzie et al (2006) "Occupancy..." p194ff

occMStime <- function(DH, occsPerSeason,
             model=NULL,
             data=NULL, ci=0.95, verify=TRUE, ...) {

  # ** DH is detection data in a 1/0/NA matrix or data frame, sites in rows,
  #    detection occasions in columns..
  # ** occsPerSeason is a scalar or vector with the number of occasions per season
  # ci is the required confidence interval.

  if(verify) {
    DH <- verifyDH(DH, allowNA=TRUE)
  } else {
    DH <- as.matrix(DH)
  }
  crit <- fixCI(ci)

  # Check for all-NA rows (eg, Grand Skinks data set!)
  allNA <- rowSums(!is.na(DH)) == 0
  if(any(allNA))  {
    DH <- DH[!allNA, ]
    data <- data[!allNA, ]
  }

  # Deal with occsPerSeason
  nOcc <- ncol(DH)
  if(length(occsPerSeason) == 1)
    occsPerSeason <- rep(occsPerSeason, nOcc/occsPerSeason)
  if(sum(occsPerSeason) != nOcc)
    stop("Detection data do not match occasions per season.")
  nseas <- length(occsPerSeason)
  seasonID <- rep(1:nseas, occsPerSeason)
  # find last season with data
  getLast <- function(dh, grp) {
    if(all(dh==0)) {  # Check for all-NA rows (eg, Grand Skinks data set!)
      return(NA)
    } else {
      return(max(which(rowsum(dh, grp) > 0)))
    }
  }
  last <- as.vector(apply((!is.na(DH))*1, 1, getLast, grp=factor(seasonID)))
  DHplus <- as.matrix(cbind(last, DH))

  # Standardise the model:
  model <- stdModel(model, defaultModel=list(gamma=~1, epsilon=~1, p=~1))

  # Check data file
  dataList <- stddata(data, NULL, 0.5)
  dataList$.interval <- as.factor(c(rep(1:(nseas-1)), NA))
  dataList$.season <- as.factor(rep(1:nseas))

  gamDf <- selectCovars(model$gamma, dataList, nseas)[-nseas, , drop=FALSE]
  gamMat <- modelMatrix(model$gamma, gamDf)
  gamK <- ncol(gamMat)
  epsDf <- selectCovars(model$epsilon, dataList, nseas)[-nseas, , drop=FALSE]
  epsMat <- modelMatrix(model$epsilon, epsDf)
  epsK <- ncol(epsMat)
  pDf <- selectCovars(model$p, dataList, nseas)
  pMat <- modelMatrix(model$p, pDf)
  pK <- ncol(pMat)
  K <- 1 + gamK + epsK + pK
  parID <- rep(1:4, c(1, gamK, epsK, pK))

  beta.mat <- matrix(NA_real_, K, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c("psi1",
    paste("gam:", colnames(gamMat)),
    paste("eps:", colnames(epsMat)),
    paste("p:", colnames(pMat)))
  lp.mat <- matrix(NA_real_, 3*nseas-1, 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  rownames(lp.mat) <- c("psi1",
    paste0("gam", 1:(nseas-1)),
    paste0("eps", 1:(nseas-1)),
    paste0("p", 1:nseas))
  logLik <- NA_real_
  npar <- NA_integer_
  varcov <- NULL

  nll <- function(param){
    psi1 <- plogis(param[1])
    gamBeta <- param[parID==2]
    epsBeta <- param[parID==3]
    pBeta <- param[parID==4]
    gamProb <- plogis(gamMat %*% gamBeta)
    epsProb <- plogis(epsMat %*% epsBeta)
    pProb <- plogis(pMat %*% pBeta)[seasonID]
    PHI0 <- c(psi1, 1-psi1)
    PHIt <- array(0, c(2, 2, nseas-1))
    PHIt[1, 1, ] <- 1 - epsProb
    PHIt[1, 2, ] <- epsProb
    PHIt[2, 1, ] <- gamProb
    PHIt[2, 2, ] <- 1 - gamProb
    Prh <- apply(DHplus, 1, Prh1A, p=pProb, PHI0=PHI0, PHIt=PHIt, seasonID)
    return(min(-sum(log(Prh)), .Machine$double.xmax))
  }

  nlmArgs <- list(...)
  nlmArgs$f <- nll
  nlmArgs$p <- rep(0, K)
  nlmArgs$hessian <- TRUE
  res <- do.call(nlm, nlmArgs)
  if(res$code > 2)   # exit code 1 or 2 is ok.
    warning(paste("Convergence may not have been reached (code", res$code, ")"))
  beta.mat[,1] <- res$estimate
  lp.mat[, 1] <- c(beta.mat[1, 1],
                   gamMat %*% beta.mat[parID==2, 1],
                   epsMat %*% beta.mat[parID==3, 1],
                   pMat %*% beta.mat[parID==4, 1])
  logLik <- -res$minimum

  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  if (!inherits(varcov0, "try-error")) {
    npar <- K
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov)))
    beta.mat[, 2] <- SE  # tidy later
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, res$estimate, "+")
    temp <- c(varcov[1, 1],
       getFittedVar(gamMat, varcov[parID==2, parID==2]),
       getFittedVar(epsMat, varcov[parID==3, parID==3]),
       getFittedVar(pMat, varcov[parID==4, parID==4]))
    if(all(temp >= 0))  {
      SElp <- sqrt(temp)
      lp.mat[, 2:3] <- sweep(outer(SElp, crit), 1, lp.mat[, 1], "+")
    }
  }

  out <- list(call = match.call(),
              beta = beta.mat,
              beta.vcv = varcov,
              real = plogis(lp.mat),
              logLik = c(logLik=logLik, df=npar, nobs=nrow(DH)))
  class(out) <- c("wiqid", "list")
  return(out)
}

# ....................................................

# A function to get Pr(dh) for a single detection history,
#   ie, one row of DH. This version has a 3-D array for PHIt

# Not exported

# dh is a 0/1/NA of length equal total no. of surveys
# p is a scalar, or vector of detection probs of length equal to
#   dh
# PHI0 is the vector c(psi1, 1-psi1)
# PHIt is a 2 x 2 x (nseas-1) array, where
#   PHIt[,,t] = matrix(c(1-eps[t], gam[t], eps[t], 1-gam[t]), 2)
# seasonID is a vector of length equal to dh, identifying the season.

Prh1A <- function(dhp, p, PHI0, PHIt, seasonID) {
  last <- dhp[1]
  if (is.na(last))  # occurs if all observations are NA
    return(1)
  dh <- dhp[-1]
  pvec <- p * dh + (1-p)*(1-dh)
  res <- PHI0
  if(last > 1)
    for(t in 1:(last-1)) {
      if(!all(is.na(pvec[seasonID==t])))  {
        D <- diag(c(prod(pvec[seasonID==t], na.rm=TRUE),
                    1-max(dh[seasonID==t], na.rm=TRUE)))
        res <- res %*% D
      }
      res <- res %*% PHIt[, , t]
    }
  PT <- c(prod(pvec[seasonID==last], na.rm=TRUE), 1-max(dh[seasonID==last], na.rm=TRUE))
  res <- res %*% PT
  return(res)
}
