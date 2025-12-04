
# Function to estimate parameters of Robust Design model

# Robust sensu Kendall et al 1997 p566
# normalising constants ignored, so AIC will not match other software
# With different p and different phi across seasons
# Same number of occasions in each season, model M[0]^t
# Returns estimates of p[i], pStar[i], phi[i], N[i], b[i]
# Precision estimation requires bootstrap: NOT YET DONE.

survRD <- function(DH, freq=1, occsPerSeason)  {

  # Do sanity checks here
  if(length(occsPerSeason) > 1)  # For the moment!
    stop("Different occasions per season are not supported: 'occsPerSeason' must be scalar.")
  # if(length(occsPerSeason) == 1)  {
    K <- ncol(DH) / occsPerSeason # Number of seasons
    # occsPerSeason <- rep(occsPerSeason, K)
  # } else {
    # K <- length(occsPerSeason)
  # }
  seasonID <- rep(1:K, each=occsPerSeason)
  if(length(seasonID) != ncol(DH))
    stop("The number of columns of 'DH' does not match the season data.")

  # turn the DH into a season-wise DH and do m-array
  getDHseason <- function(dh)
    tapply(dh, as.factor(seasonID), max)
  DHseason <- t(apply(DH, 1, getDHseason))
  mMat <- ch2mArray(DHseason, freq=freq)

  # For L2, we need only the count of each within-season capture history.
  getDHchar <- function(dh)
    tapply(dh, as.factor(seasonID), paste, collapse="")
  DHchar <- t(apply(DH, 1, getDHchar))

  # What capture histories are possible?
  # This version assumes nOcc same for all seasons ######
  nOcc <- occsPerSeason[1]
  Omega01 <- matrix(0, 2^nOcc, nOcc)
  for(i in 1:nOcc)
    Omega01[, i] <- rep(0:1, each=2^(nOcc-i))
  OmegaCh <- apply(Omega01, 1, paste0, collapse="")

  # Get the count of each capture history for each season
  getX.i <- function(x) {
    tmp <- factor(x, levels=OmegaCh)
    table(tmp)[-1]
  }
  X.i <- apply(DHchar, 2, getX.i)

  # Function to calculate a vector of probabilities for capture histories
  # (with constant p)
  getpCond <- function(Omega01, p) {
    tmp <- Omega01 * p + (1 - Omega01) * (1 - p)
    pOmega <- apply(tmp, 1, prod)[-1]
    return(pOmega / sum(pOmega))
  }

  param <- rep(0, K*2 - 1)
  nll <- function(param)  {
    p <- plogis(param[1:K])
    pStar <- 1 - (1 - p)^nOcc
    phi <- plogis(param[(K+1):(K*2-1)])
    # logL1 <- sum(mMat * log(qArray(phi, pStar[-1])), na.rm=TRUE)
    logL1 <- sum(mMat * log_qArray(log(phi), log(pStar[-1]), log(1 - pStar[-1])))
    logL2i <- numeric(K)
    for(i in 1:K) {
      pCond <- getpCond(Omega01, p[i])
      logL2i[i] <- sum(X.i[, i] * log(pCond))
    }
    return(min(-logL1 - sum(logL2i), .Machine$double.xmax))
  }

  res <- nlm(nll, param)  ## , hessian=TRUE)
  res

  pHat <- plogis(res$estimate[1:K])
  phiHat <- plogis(res$estimate[(K+1):(2*K-1)])

  pStarHat <- 1 - (1 - pHat)^nOcc  # return this
  n <- as.vector(colSums(DHseason))
  Nhat <- n / pStarHat # return this


  # Kendal et al 1995 Eqns 2 and 3:
  # Btilde <- Nhat[-1] - phiHat * Nhat[-K]
  bHat <- Nhat[-1] / Nhat[-K] - phiHat   # return this

  return(list(
    phiHat = phiHat,
    bHat = bHat,
    pStarHat = pStarHat,
    Nhat = Nhat,
    pHat = pHat))
}
