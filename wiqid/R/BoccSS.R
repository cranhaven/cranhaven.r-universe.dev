# Single season occupancy with site and survey covariates.

# Bayesian version, using Dorazio & Rodriguez (2011) algorithm

BoccSS <- function(DH, model=NULL, data=NULL, priors=list(),
                    chains=3, draws=30000, burnin=1000, thin=1, parallel,
                    seed=NULL, doWAIC=FALSE, ...) {
  # single-season occupancy models with site and survey covariates
  # ** DH is detection data in a 1/0/NA matrix or data frame, sites in rows,
  #    detection occasions in columns..
  # ** model is a list of 2-sided formulae for psi and p; can also be a single
  #   2-sided formula, eg, model = psi ~ habitat.
  # ** data is a DATA FRAME with single columns for site covariates and a column for each survey occasion for each survey covariate.
  # ** priors is a list with elements for prior mean and variance for coefficients.
  startTime <- Sys.time()

  if(!is.null(list(...)$sample)) {
    message("* The 'sample' argument is deprecated, please use 'draws'. *")
    draws <- list(...)$sample
  }

  # Check DH:
  tst <- try(range(DH, na.rm=TRUE), silent=TRUE)
  if(inherits(tst, "try-error") || tst[1] < 0 || tst[2] > 1)
    stop("DH is not a valid detection history matrix (or data frame).")

  # Deal with parallel (order of the if statements is important!)
  if(chains == 1)
    parallel <- FALSE
  if(missing(parallel))
    parallel <- chains < detectCores()
  if(parallel) {
    coresToUse <- min(chains, detectCores() - 1)
    if(coresToUse < 2) {
      warning("Multiple cores not available; running chains sequentially.")
      parallel <- FALSE
    }
  }
  if(parallel) {
    if(chains > coresToUse)
      warning(paste("Running", chains, "chains on", coresToUse, "cores."))
    cl <- makeCluster(coresToUse)
    on.exit(stopCluster(cl))
  }

  # Standardise the model:
  model <- stdModel(model, list(psi=~1, p=~1))

  # Summarize detection history
  site.names <- rownames(DH)
  DH <- as.matrix(DH)
  nSites <- nrow(DH)
  nSurv <- ncol(DH)
  if (nSurv < 2)
    stop("More than one survey occasion is needed")
  if(is.null(site.names))
    site.names <- 1:nSites

  # Convert the covariate data frame into a list
  dataList <- stddata(data, nSurv, scaleBy = 1)
  time <- rep(1:nSurv, each=nSites)
  dataList$.Time <- standardize(time)
  dataList$.Time2 <- dataList$.Time^2
  dataList$.Time3 <- dataList$.Time^3
  dataList$.time <- as.factor(time)
  before <- cbind(0L, DH[, 1:(nSurv - 1)]) # 1 if species seen on previous occasion
  dataList$.b <- as.vector(before)

  survey.done <- !is.na(as.vector(DH))
  DHvec <- as.vector(DH)[survey.done]
  siteID <- row(DH)[survey.done]  # need both numeric vector
  siteIDfac <- as.factor(siteID)  #    and factor (for tapply)
  # survID <- col(DH)[survey.done]

  psiDf <- selectCovars(model$psi, dataList, nSites)
  if (nrow(psiDf) != nSites)
    stop("Number of site covars doesn't match sites.\nAre you using survey covars?")
  psiModMat <- modelMatrix(model$psi, psiDf)
  if(nrow(psiModMat) != nrow(psiDf))
      stop("Missing site covariates are not allowed.")
  psiK <- ncol(psiModMat)
  pDf0 <- selectCovars(model$p, dataList, nSites*nSurv)
  pDf <- pDf0[survey.done, , drop=FALSE]
  pModMat <- modelMatrix(model$p, pDf)
  if(nrow(pModMat) != nrow(pDf))
      stop("Missing survey covariates are not allowed when a survey was done.")
  pK <- ncol(pModMat)
  K <- psiK + pK

  # Organise and check priors
  if(!is.null(priors))  {
    priorErrorFlag <- FALSE
    priorsDefault <- list(muPsi=0, sigmaPsi=1, muP=0, sigmaP=1)
    priors <- replace (priorsDefault, names(priors), priors)
    ### TODO ### check for NAs and sigma <= 0
    muPsi <- priors$muPsi
    if(length(muPsi) == 1)
      muPsi <- rep(muPsi, psiK)
    if(length(muPsi) != psiK) {
      message("Wrong length for priors$muPsi, should have values for:")
      message(paste(colnames(psiModMat), collapse=" "))
      priorErrorFlag <- TRUE
    }
    sigmaPsi <- priors$sigmaPsi
    if(!is.matrix(sigmaPsi)) {
      if(length(sigmaPsi) == 1)
        sigmaPsi <- rep(sigmaPsi, psiK)
      sigmaPsi <- diag(sigmaPsi, nrow=psiK)
    }
    if(ncol(sigmaPsi) != psiK || nrow(sigmaPsi) != psiK) {
      message("Wrong dimensions for priors$sigmaPsi, should have values for:")
      message(paste(colnames(psiModMat), collapse=" "))
      priorErrorFlag <- TRUE
    }
    muP <- priors$muP
    if(length(muP) == 1)
      muP <- rep(muP, pK)
    if(length(muP) != pK) {
      message("Wrong length for priors$muP, should have values for:")
      message(paste(colnames(pModMat), collapse=" "))
      priorErrorFlag <- TRUE
    }
    sigmaP <- priors$sigmaP
    if(!is.matrix(sigmaP)) {
      if(length(sigmaP) == 1)
        sigmaP <- rep(sigmaP, pK)
      sigmaP <- diag(sigmaP, nrow=pK)
    }
    if(ncol(sigmaP) != pK || nrow(sigmaP) != pK) {
      message("Wrong dimensions for priors$sigmaP, should have values for:")
      message(paste(colnames(pModMat), collapse=" "))
      priorErrorFlag <- TRUE
    }
    if(priorErrorFlag)
      stop("Invalid prior specification")
  }

  # Run MLE to get starting values
  # Negative log likelihood function
  nll <- function(param){
    psiBeta <- param[1:psiK]
    pBeta <- param[(psiK+1):K]
    psiProb <- as.vector(pnorm(psiModMat %*% psiBeta))
    pProb <- pnorm(pModMat %*% pBeta)
    Lik1 <- DHvec*pProb + (1-DHvec) * (1-pProb)
    Lik2 <- tapply(Lik1, siteIDfac, prod)
    llh <- sum(log(psiProb * Lik2 +
          (1 - psiProb) * (rowSums(DH, na.rm=TRUE) == 0)))
    return(min(-llh, .Machine$double.xmax))
  }

  # Run mle estimation with nlm:
  param <- rep(0, K)
  mle <- nlm(nll, param)$estimate

  # Gibbs sampler variables
  XprimeX <- t(psiModMat) %*% psiModMat
  if(is.null(priors)) {
    V.beta <- chol2inv(chol(XprimeX))
    ScaledMuPsi <- 0
    SigmaInvP <- 0
    ScaledMuP <- 0
  } else {
    SigmaInvPsi = chol2inv(chol(sigmaPsi))
    V.beta = chol2inv(chol(SigmaInvPsi + XprimeX)) # prior here
    ScaledMuPsi = SigmaInvPsi %*% muPsi
    SigmaInvP = chol2inv(chol(sigmaP))
    ScaledMuP = SigmaInvP %*% muP
  }
  # Starting values - use MLEs
  set.seed(seed)
  starters <- vector('list', chains)
  for(i in 1:chains)
    starters[[i]] <- mle * runif(K, 0.95, 1.05) #####
  y <- rowSums(DH, na.rm=TRUE)
  z <- as.integer(y > 0)  ## (Starting value for) occupancy state

  n.iter <- ceiling(draws / chains) * thin + burnin
  message("Starting MCMC run for ", chains, " chains with ", n.iter, " iterations.")
  flush.console()

  # Function to do 1 chain
  run1chain <- function(start) {
    beta <- matrix(start[1:psiK], ncol=1)  # why matrix?
    alpha <- matrix(start[-(1:psiK)], ncol=1)
    if(doWAIC) {
      chain <- matrix(nrow=n.iter, ncol=K+nSites) # to hold MCMC chains
      colnames(chain) <- c(paste("psi", colnames(psiModMat), sep="_"),
                        paste("p", colnames(pModMat), sep="_"), site.names)
    } else {
      chain <- matrix(nrow=n.iter, ncol=K) # to hold MCMC chains
      colnames(chain) <- c(paste("psi", colnames(psiModMat), sep="_"),
                        paste("p", colnames(pModMat), sep="_"))
    }
    v <- rep(NA, psiK)
    for (draw in 1:n.iter) {
      #  draw z for sites with y = 0, using conditional (on data) prob of occupancy
      psi <- as.vector(pnorm(psiModMat %*% beta))
      q <- 1 - as.vector(pnorm(pModMat %*% alpha))
      pMissed <- tapply(q, siteIDfac, prod)
      z.prob <- pmin(psi * pMissed / (psi * pMissed  + 1 - psi), 1)
         # if psi close to 1, z.prob comes out to > 1.
      z <- ifelse(y, 1, rbinom(length(y), size=1, prob=z.prob))
      present <- z == 1

      # draw v and beta ## coefficients for psi
      vmean <- as.vector(psiModMat %*% beta) # same as 'psi' above
      v[present]  <- truncnorm::rtruncnorm(sum(present),  a=0, mean=vmean[present], sd=1)
      if(!all(present))
        v[!present] <- truncnorm::rtruncnorm(sum(!present), b=0, mean=vmean[!present], sd=1)
      betaMean = V.beta %*% (ScaledMuPsi + (t(psiModMat) %*% v) ) # prior here
      beta <- matrix(MASS::mvrnorm(1, betaMean, V.beta), ncol=1)

      # draw u and alpha ## coefficients for p
      ## This section ONLY needs data from occupied sites, ie, z == 1, present == TRUE
      needed <- present[siteID]
      DHneeded <- DHvec[needed]
      Wmat <- pModMat[needed, , drop=FALSE]
      umean <- as.vector(Wmat %*% alpha)
      u <- rep(NA, nrow(Wmat))
      ind.y <- DHneeded == 1
      u[ind.y]  <- truncnorm::rtruncnorm(sum(ind.y), a=0, mean=umean[ind.y], sd=1)
      u[!ind.y] <- truncnorm::rtruncnorm(sum(!ind.y), b=0, mean=umean[!ind.y], sd=1)

      WprimeW <- t(Wmat) %*% Wmat
      V.alpha = chol2inv(chol(SigmaInvP + WprimeW)) # prior here

      alphaMean = V.alpha %*% (ScaledMuP + (t(Wmat) %*% u) ) # prior here
      alpha <- matrix(MASS::mvrnorm(1, alphaMean, V.alpha), ncol=1)

      # Calculate ppd for each site
      if(doWAIC) {
        p.vec <- as.vector(pnorm(pModMat %*% alpha))
        lik1 <- dbinom(DHvec, 1, p.vec)
        lik2 <- tapply(lik1, siteIDfac, prod)
        ppd <- psi * lik2 + (1 - psi) * (y == 0)
        chain[draw, ] <- c(beta, alpha, ppd)
      } else {
        chain[draw, ] <- c(beta, alpha)
      }
    }
    return(coda::mcmc(chain[(burnin+1):n.iter, ], thin=thin))
  }

  if(parallel) {
    clusterExport(cl, c("K", "psiK", "n.iter", "burnin", "thin",
        "psiModMat", "pModMat", "siteID", "siteIDfac", "y", "DHvec",
        "ScaledMuPsi", "V.beta", "SigmaInvP", "ScaledMuP", "doWAIC"),
        envir=environment())
    clusterSetRNGStream(cl, seed)
    chainList <- parLapply(cl, starters, run1chain)
  } else {
    chainList <- lapply(starters, run1chain)
  }
  message("MCMC run complete.")
  MCMC <- mcmc.list(chainList)
  if(doWAIC) {
    ppd <- as.matrix(MCMC[, -(1:K)])
    MCMC <- MCMC[, 1:K]
    lppd <- log(ppd)
    tmp.sum <- -2 * sum(log(colMeans(ppd)))  # first term of eqn 45
    pD <- sum(apply(lppd, 2, var)) # eqn 44
    WAIC <- tmp.sum + 2 * pD
  }

  out <- mcmcOutput(MCMC,
      header = "Model fitted in R with a Gibbs sampler")
  attr(out, "call") <- match.call()
  if(doWAIC)
    attr(out, "WAIC") <- c(WAIC = WAIC, pD = pD)
  attr(out, "timeTaken") <- unclass(difftime(Sys.time(), startTime, units="secs"))
  return(out)
}

