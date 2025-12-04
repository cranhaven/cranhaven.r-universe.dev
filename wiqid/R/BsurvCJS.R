

# Bayesian version of CJS models

# This uses rjags via the local justRunJags function

BsurvCJS <- function(DH, model=list(phi~1, p~1), data=NULL, freq=1, priors=NULL,
    chains=3, draws=1e4, burnin=1000, thin=1, adapt=1000,
    parallel = NULL, seed=NULL, priorOnly=FALSE, ...) {
  # phi(t) p(t) model or models with time covariates for Cormack-Joly-Seber
  # estimation of apparent survival.
  # ** DH is detection history matrix/data frame, animals x occasions.
  # ** freq is vector of frequencies for each detection history
  # ** model is a list of 2-sided formulae for psi and p; can also be a single
  #   2-sided formula, eg, model = psi ~ habitat.
  # ** data a data frame with the covariates.
  # ** ci is required confidence interval.

  startTime <- Sys.time()

  if(!is.null(list(...)$sample)) {
    message("*The 'sample' argument is deprecated, please use 'draws'.*")
    draws <- list(...)$sample
  }

  # Sanity checks:
  if (priorOnly)
    warning("The prior distributions will be produced, not the posterior distributions!")

  ni <- ncol(DH) - 1  # number of survival intervals and REcapture occasions
  if(!is.null(data) && nrow(data) != ni)
    stop("'DH' and 'data' must have the same number of rows.")

  # Convert detection history to m-array to facilitate use of multinomial likelihood
  mArray <- ch2mArray(CH=DH, freq=freq)

  # Standardise the model:
  model <- stdModel(model, defaultModel=list(phi=~1, p=~1))

  # Standardize the data
  dataList <- stddata(data, NULL)
  dataList$.Time <- standardize(1:ni)
  dataList$.Time2 <- dataList$.Time^2
  dataList$.Time3 <- dataList$.Time^3
  dataList$.time <- as.factor(1:ni)

  # Set up model matrices
  phiDf <- selectCovars(model$phi, dataList, ni)
  phiMat <- modelMatrix(model$phi, phiDf)
  phiK <- ncol(phiMat)
  pDf <- selectCovars(model$p, dataList, ni)
  pMat <- modelMatrix(model$p, pDf)
  pK <- ncol(pMat)
  K <- phiK + pK
  if(nrow(phiMat) != ni || nrow(pMat) != ni)
    stop("Missing values not allowed in covariates.")

  # Deal with priors:
  defaultPriors <- list(muPhi = rep(0, phiK),
                        sigmaPhi = rep(1, phiK),
                        muP = rep(0, pK),
                        sigmaP = rep(1, pK))
  priors <- checkPriors(priors, defaultPriors)

  # Run MLE version to get starting values
  nll <- function(param){
    phiBeta <- param[1:phiK]
    pBeta <- param[(phiK+1):K]
    log_phi <- plogis(phiMat %*% phiBeta, log.p=TRUE)
    link_p <- pMat %*% pBeta
    log_p <- plogis(link_p, log.p=TRUE)
    log_1mp <- plogis( -link_p, log.p=TRUE)
    # Output the negative log(likelihood) value:
    nll <-  -sum(mArray * log_qArray(log_phi, log_p, log_1mp))
    return(min(nll, .Machine$double.xmax))
  }

  # Run mle estimation with nlm:
  param <- rep(0, K)
  res <- nlm(nll, param)
  # if(res$code > 2)   # exit code 1 or 2 is ok.
    # stop("MLE estimation failed.")
  start <- res$estimate

  # Do the model:
  modelFile <- file.path(tempdir(), "JAGSmodel.txt")
  modeltext <- "
    model{
      # priors for beta parameters
      for(i in 1:phiK)  {
        phiBeta[i] ~ dnorm(muPhi[i], tauPhi[i])
      }
      for(i in 1:pK)  {
        pBeta[i] ~ dnorm(muP[i], tauP[i])
      }
      # Calculate p and phi
      for(t in 1:(nocc-1)) {
        probit(phi[t]) <- sum(phiBeta[] * phiMat[t, ])
        probit(p[t]) <- sum(pBeta[] * pMat[t, ])
      }
      # Multinomial likelihood
      for(t in 1:(nocc-1)) {
        marr[t, 1:nocc] ~ dmulti(pr[t, ], rel[t])
      }
      # Cell probs of the m-array
      for(t in 1:(nocc-1)) {
        q[t] <- 1 - p[t]  # prob of non-recapture
        # main diagonal
        pr[t, t] <- phi[t] * p[t]
        # above main diagonal
        for(j in (t+1):(nocc-1)) {
          pr[t, j] <- prod(phi[t:j]) * prod(q[t:(j-1)]) * p[j]
        }
        # below main diagonal
        for(j in 1:(t-1)) {
          pr[t, j] <- 0
        }
      }
      # last column, prob of never recaptured
      for(t in 1:(nocc-1)) {
        pr[t, nocc] <- 1 - sum(pr[t, 1:(nocc-1)])
      }
    }
  "
  writeLines(modeltext, con=modelFile)

   # organise the data:
  jagsData <- list(nocc = ncol(mArray), rel=rowSums(mArray),
                      pK = pK, phiK = phiK, pMat=pMat, phiMat=phiMat,
                      muPhi = priors$muPhi, tauPhi = 1/(priors$sigmaPhi)^2,
                      muP = priors$muP, tauP = 1/(priors$sigmaP)^2)
  if(!priorOnly)
      jagsData$marr <- mArray

  inits <- function() {
    start1 <- start * runif(K, 0.9, 1.1)
    list(phiBeta = start1[1:phiK], pBeta = start1[(phiK+1):K])
  }
  wanted <- c("phi", "p")

  # Run the model:
  resB <- justRunJags(jagsData, inits, wanted, modelFile,
            chains, draws, burnin, thin, adapt,
            modules = c("glm"), parallel = parallel, seed=seed)

  out <- mcmcOutput(resB,
      header = "Model fitted in JAGS with 'rjags' functions")
  attr(out, "call") <- match.call()
  attr(out, "timeTaken") <- unclass(difftime(Sys.time(), startTime, units="secs"))
  return(out)
}
