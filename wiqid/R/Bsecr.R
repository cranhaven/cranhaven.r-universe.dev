
# Bayesian version of secr to work with stoats data

Bsecr0 <- function(capthist, buffer = 100, start=NULL, nAug = NA,
                    maxSig = 2*buffer,
                    chains=3, draws=1e4, burnin=0, thin=1, adapt=1000,
                    priorOnly=FALSE, parallel=NULL, seed=NULL, ...) {

  if(!is.null(list(...)$sample)) {
    message("*The 'sample' argument is deprecated, please use 'draws'.*")
    draws <- list(...)$sample
  }
  if(!inherits(capthist, "capthist"))
    stop("'capthist' is not a valid capthist object.")
  if(length(dim(capthist)) < 3)
    stop("'capthist' object not compatible with secr v.3, please upgrade.")

  startTime <- Sys.time()

  if (priorOnly)
    warning("The prior distributions will be produced, not the posterior distributions!")

  traps <- secr::traps(capthist)
  J <- nrow(traps)
  xl <- min(traps$x) - buffer
  xu <- max(traps$x) + buffer
  yl <- min(traps$y) - buffer
  yu <- max(traps$y) + buffer
  A <- (xu-xl)*(yu-yl) / 1e4 # ha

  # Get starting values, etc from secr.fit
  if(!is.null(start) && inherits(start, "secr")) {
    mle.res <- predict(start)[, 2]
  } else {
    mask <- secr::make.mask(traps, buffer)
    mle.res <- unlist(secr::autoini(capthist, mask))
  }

  if(is.na(nAug))
    nAug <- ceiling(3 * mle.res[1] * A)
  psistart  <- (mle.res[1] * A) / nAug

  # Convert capture histories into an Animals x Traps matrix
  nInd <- dim(capthist)[1]
  nOcc <- dim(capthist)[2]
  yMat <- matrix(0, nAug, J)
  yMat[1:nInd,] <- apply(capthist, c(1,3), sum)

  # Get initial locations of animals
  SX <- SY <- numeric(nInd)
  for(i in 1:nInd) {
    where <- colMeans(traps[which(yMat[i, ] > 0), , drop=FALSE])
    SX[i] <- where[1]
    SY[i] <- where[2]
  }

  # Define the model
  modelFile <- file.path(tempdir(), "JAGSmodel.txt")
  modeltext <- "
    model {
    sigma ~ dunif(0, maxSig)      # need to set good max
    sigma2 <- 2*sigma^2
    g0 ~ dunif(0, 1)
    psi ~ dunif(0, 1)
    for (i in 1:M){         #loop through the augmented population
      z[i] ~ dbern(psi)     #state of individual i (real or imaginary)
      SX[i] ~ dunif(xl, xu) #priors for the activity centre for each individual
      SY[i] ~ dunif(yl, yu) #  xl, yl = lower coordinate; xu, yu = upper value
      for(j in 1:J) {       #loop through the J detector locations
         Dsq[i,j] <- pow(SX[i]-trapmat[j,1], 2) + pow(SY[i]-trapmat[j,2],2)
         g[i,j] <- z[i] * g0 * exp(-Dsq[i,j]/sigma2)
         y[i,j] ~ dbin(g[i,j], K)
      }
    }
    N <- sum(z[1:M]) # derive number (check against M)
    D <- N / A       # derive density
  }   "
  writeLines(modeltext, con=modelFile)

   # organise the data:
  jagsData <- list(M = nAug, xl=xl, xu=xu, yl=yl, yu=yu, J=J,
      trapmat=as.matrix(traps), K=nOcc,
      A = A, maxSig = maxSig)
  if (!priorOnly)
    jagsData$y <- yMat
  inits <- function() {list(z=rep(1, nAug),
                        SX=c(SX, runif(nAug-nInd, xl, xu)),
                        SY=c(SY, runif(nAug-nInd, yl, yu)),
                        sigma=mle.res[3], g0=mle.res[2],
                        psi=psistart)}
  wanted <- c("D", "g0", "sigma", "SX", "SY", "z")

  # Run the model:
  resB <- justRunJags(jagsData, inits, wanted, modelFile,
            chains, draws, burnin, thin, adapt,
            modules = c("glm"), parallel = parallel, seed=seed)
  resMat <- as.matrix(resB)
  forB <- colnames(resMat) == "D" | colnames(resMat) == "g0" | colnames(resMat) == "sigma"

  AC0 <- resMat[, !forB][, 1:(nAug*2)]
  dim(AC0) <- c(dim(AC0)[1], nAug, 2)
  w <- resMat[, !forB][, nAug*2 + 1:nAug]
  w[w==0] <- NA
  AC <- sweep(AC0, 1:2, w, "*")

  animalIDs <- sprintf("id%03d", 1:nAug)
  aid <- dimnames(capthist)[[1]]
  if(!is.null(aid))
    animalIDs[1:length(aid)] <- aid
  dimnames(AC) <- list(NULL, animalIDs, c("x", "y"))

  out <- mcmcOutput(resMat[, forB], nChains=chains,
      header = "Model fitted in JAGS with 'rjags' functions")
  attr(out, "ACs") <- AC
  attr(out, "traps") <- secr::traps(capthist)
  attr(out, "timeTaken") <- unclass(difftime(Sys.time(), startTime, units="secs"))
  attr(out, "call") <- match.call()
  # check augmentation
  if(ceiling(max(out$D) * A) >= nAug)
    warning(paste("Augmentation may not be adequate; rerun with nAug >>", nAug))
  return(out)
}
