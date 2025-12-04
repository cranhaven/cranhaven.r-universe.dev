
# Function to estimate parameters of Robust Design model
#  with "ad hoc" strategy:
# N and p are estimated beforehand with a closedCap* function


survRDah <- function(DH, freq=1, occsPerSeason, N, pStar)  {

  # Do sanity checks here
  if(length(occsPerSeason) > 1)  # For the moment!
    stop("Different occasions per season are not supported: 'occsPerSeason' must be scalar.")

  K <- ncol(DH) / occsPerSeason # Number of seasons
  if(length(N) != K)
    stop("'N' must have one value per season.")
  if(length(pStar) != K)
    stop("'pStar' must have one value per season.")
  seasonID <- rep(1:K, each=occsPerSeason)
  if(length(seasonID) != ncol(DH))
    stop("The number of columns of 'DH' does not match the season data.")

  # turn the DH into a season-wise DH and do m-array
  getDHseason <- function(dh)
    tapply(dh, as.factor(seasonID), max)
  DHseason <- t(apply(DH, 1, getDHseason))
  mMat <- ch2mArray(DHseason, freq=freq)

  param <- rep(0, K-1)
  # Log likelihood function
  nll <- function(param)  {
    log_phi <- plogis(param, log.p=TRUE)
    nll <- -sum(mMat * log_qArray(log_phi, log(pStar[-1]), log(1 - pStar[-1])))
    return(min(nll, .Machine$double.xmax))
  }

  res <- nlm(nll, param)  ## , hessian=TRUE)
  phiHat <- plogis(res$estimate)

  bHat <- N[-1] / N[-K] - phiHat   # return this

  return(list(
    phiHat = phiHat,
    bHat = bHat,
    pStarHat = pStar,
    Nhat = N))
}
