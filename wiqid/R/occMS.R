
# Multiseason occupancy - version cov2
# This version allows for
#  1. site covars for psi1
#  2. site and site x interval covars (including a fixed interval effect) for gamma and epsilon
#  3. site and site x survey occasion covars for probability of detection.

# See MacKenzie et al (2006) "Occupancy..." p194ff

occMS <- function(DH, occsPerSeason,
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

  # Check for simple models:
  if(is.null(model))
    return(occMS0(DH=DH, occsPerSeason=occsPerSeason, ci=ci, verify=FALSE, ...))
  if(is.null(data))
    return(occMStime(DH=DH, occsPerSeason=occsPerSeason, model=model, data=NULL,
      ci=ci, verify=FALSE, ...))

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
  # DHplus <- as.matrix(cbind(last, DH))

  # Extract info on surveys done, ie, 1 or 0 in the DH
  survey.done <- !is.na(as.vector(DH))
  DHvec <- as.vector(DH)[survey.done]
  siteID <- row(DH)[survey.done]
  survID <- col(DH)[survey.done]

  # Standardise the model:
  model <- stdModel(model, defaultModel=list(psi1=~1, gamma=~1, epsilon=~1, p=~1))

  # Check data file
  nSites <- nrow(DH)
  siteNames <- rownames(DH)
  if (is.null(siteNames))
    siteNames <- rownames(data)
  if (is.null(siteNames))
    siteNames <- 1:nSites

  if(!is.null(data)) {
    if(nrow(data) != nSites)
      stop("data must have a row for each site")
    rownames(data) <- NULL  # rownames cause problems when the data frame is recast
  }
  dataList <- stddata(data, c(nOcc, nseas - 1), scaleBy=NULL)
  # add built-in covars
  interval <- rep(1:(nseas-1), each=nSites)
  dataList$.interval <- as.factor(interval)
  occasion <- rep(1:nOcc, each=nSites)
  dataList$.occasion <- as.factor(occasion)
  season <- rep.int(1:nseas, nSites*occsPerSeason)
  dataList$.season <- as.factor(season)
  # Get factor levels and scaling values (needed for prediction)
  xlev <- lapply(dataList[sapply(dataList, is.factor)], levels)
  scaling <- lapply(dataList[sapply(dataList, is.numeric)],
    getScaling, scaleBy = 1)
  dataList <- lapply(dataList, doScaling, scaleBy = 1)

  psi1df <- selectCovars(model$psi1, dataList, nSites)
  psi1Mat <- modelMatrix(model$psi1, psi1df)
  psi1K <- ncol(psi1Mat)
  gamDf <- selectCovars(model$gamma, dataList, nSites*(nseas-1))
  gamMat <- modelMatrix(model$gamma, gamDf)
  gamK <- ncol(gamMat)
  epsDf <- selectCovars(model$epsilon, dataList, nSites*(nseas-1))
  epsMat <- modelMatrix(model$epsilon, epsDf)
  epsK <- ncol(epsMat)
  pDfNA <- selectCovars(model$p, dataList, nSites*nOcc)
  pDf <- pDfNA[survey.done, , drop=FALSE]
  pMat <- modelMatrix(model$p, pDf)  # modelMatrix removes any NAs
  if (nrow(pMat) != sum(survey.done))
    stop("Missing survey covars not allowed when a survey was done.")
  pK <- ncol(pMat)
  K <- psi1K + gamK + epsK + pK
  parID <- rep(1:4, c(psi1K, gamK, epsK, pK))
  index <- vector('list', length(model)) # needed for 'predict'
  names(index) <- names(model)
  for(i in seq_along(model))
    index[[i]] <- (1:K)[parID == i]

  # objects to hold the output
  beta.mat <- matrix(NA_real_, K, 4)
  colnames(beta.mat) <- c("est", "SE", "lowCI", "uppCI")
  rownames(beta.mat) <- c(
    paste("psi1:", colnames(psi1Mat)),
    paste("gam:", colnames(gamMat)),
    paste("eps:", colnames(epsMat)),
    paste("p:", colnames(pMat)))
  lp.mat <- matrix(NA_real_, nSites*(nseas*2 - 1) + sum(survey.done), 3)
  colnames(lp.mat) <- c("est", "lowCI", "uppCI")
  rownames(lp.mat) <- c(
    paste0("psi:", siteNames),
    paste0("gamma:", siteNames, ",", interval),
    paste0("epsilon:", siteNames, ",", interval),
    paste0("p:", siteNames[siteID], ",", survID))
  logLik <- NA_real_
  npar <- NA_integer_
  varcov <- NULL

  # negative log likelihood function
  nll <- function(param){
    psi1Beta <- param[parID==1]
    gamBeta <- param[parID==2]
    epsBeta <- param[parID==3]
    pBeta <- param[parID==4]
    psi1Prob <- plogis(psi1Mat %*% psi1Beta)
    gamProb <- matrix(plogis(gamMat %*% gamBeta), nrow=nSites)
    epsProb <- matrix(plogis(epsMat %*% epsBeta), nrow=nSites)
    pProb <- plogis(pMat %*% pBeta)
    Prh <- rep(1, nSites)
    for(i in 1:nSites) {
      if (is.na(last[i]))
        next
      res <- c(psi1Prob[i], 1-psi1Prob[i]) # aka PHIO
      PHIt <- array(0, c(2, 2, nseas-1))
      PHIt[1, 1, ] <- 1 - epsProb[i, ]
      PHIt[1, 2, ] <- epsProb[i, ]
      PHIt[2, 1, ] <- gamProb[i, ]
      PHIt[2, 2, ] <- 1 - gamProb[i, ]
      p <- pProb[siteID == i]
      dh <- DH[i, survID[siteID == i]]
      pvec <- p * dh + (1-p)*(1-dh)
      whichSeas <- seasonID[survID[siteID == i]]
      if(last[i] > 1)
        for(t in 1:(last[i]-1)) {
          if(any(whichSeas == t))  {
            D <- diag(c(prod(pvec[whichSeas==t]), 1-max(dh[whichSeas==t])))
            res <- res %*% D
          }
          res <- res %*% PHIt[, , t]
        }
      PT <- c(prod(pvec[whichSeas==last[i]]), 1-max(dh[whichSeas==last[i]]))
      Prh[i] <- res %*% PT
    }
    return(min(-sum(log(Prh)), .Machine$double.xmax))
  }

  # res <- nlm(nll, start, hessian=TRUE)
  nlmArgs <- list(...)
  nlmArgs$f <- nll
  nlmArgs$p <- rep(0, K)
  nlmArgs$hessian <- TRUE
  res <- do.call(nlm, nlmArgs)

  if(res$code > 2)   # exit code 1 or 2 is ok.
    warning(paste("Convergence may not have been reached (code", res$code, ")"))
  beta.mat[,1] <- res$estimate
  lp.mat[, 1] <- c(psi1Mat %*% beta.mat[parID==1, 1],
                   gamMat %*% beta.mat[parID==2, 1],
                   epsMat %*% beta.mat[parID==3, 1],
                   pMat %*% beta.mat[parID==4, 1])
  logLik <- -res$minimum

  varcov0 <- try(chol2inv(chol(res$hessian)), silent=TRUE)
  # if (!inherits(varcov0, "try-error") && all(diag(varcov0) > 0)) {
  if (!inherits(varcov0, "try-error")) {
    npar <- K
    varcov <- varcov0
    SE <- suppressWarnings(sqrt(diag(varcov)))
    beta.mat[, 2] <- SE  # tidy later
    beta.mat[, 3:4] <- sweep(outer(SE, crit), 1, res$estimate, "+")
    temp <- c(
       # diag(psi1Mat %*% varcov[parID==1, parID==1] %*% t(psi1Mat)),
       getFittedVar(psi1Mat, varcov[parID==1, parID==1]),
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
              logLik = c(logLik=logLik, df=npar, nobs=nrow(DH)),
              ci = ci,
              formulae = model,
              index = index,
              xlev = xlev,
              scaling = scaling)
  class(out) <- c("wiqid", "list")
  return(out)
}
