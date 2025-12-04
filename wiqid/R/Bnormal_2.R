
# Bayesian modelling of normal distribution with JAGS
# ===================================================
# This version allows for a gamma prior for the scale, sigma,
#  though the default is a uniform prior over a wide range.
# The gamma prior is specified by mode and SD.
# Similar to the one-sample version of BEST::BESTmcmc
#  but with normal instead of t-distribution.

Bnormal2 <- function(y, priors=NULL,
                    chains=3, draws=3e4, burnin=0, thin=1, adapt=1000,
                    doPriorsOnly=FALSE, parallel=NULL, seed=NULL, ...) {

   if(!is.null(list(...)$sample)) {
    message("* The 'sample' argument is deprecated, please use 'draws'. *")
    draws <- list(...)$sample
  }
 startTime <- Sys.time()

  if(doPriorsOnly)
    warning("The output shows the prior distributions,
      NOT the posterior distributions for your data.")

  # Data checks
  if(!all(is.finite(y)))
    stop("The input data include NA or Inf.")
  if(length(unique(y)) < 2 &&      # sd(y) will be 0 or NA; ok if priors specified.
        (is.null(priors) ||
          is.null(priors$muSD) ||
          is.null(priors$sigmaMode) ||
          is.null(priors$sigmaSD)))
  stop("If priors are not specified, data must include at least 2 (non-equal) values.")

  # Prior checks:
  if(!is.null(priors))  {
    if(!is.list(priors)) {
        stop("'priors' must be a list (or NULL).")
    }
    nameOK <- names(priors) %in%
          c("muMean", "muSD", "sigmaMode", "sigmaSD")
    if(!all(nameOK))
      stop("Invalid items in prior specification: ",
          paste(sQuote(names(priors)[!nameOK]), collapse=", "))
    if(!all(sapply(priors, is.numeric)))
      stop("All items in 'priors' must be numeric.")
    if(!is.null(priors$muSD) && priors$muSD <= 0)
      stop("muSD must be > 0")
  }

  # THE PRIORS
  if(is.null(priors$muMean))
    priors$muMean <-  mean(y)
  if(is.null(priors$muSD))
    priors$muSD <-  sd(y) * 1000
  if(mean(y) > priors$muMean + priors$muSD || mean(y) < priors$muMean - priors$muSD)
    warning("Sample mean is outside the prior range mMean \u00B1 sMean.")
  priors$muP <- 1/priors$muSD^2 # convert to precision
  priors$muSD <- NULL           #   and remove SD

  useUniformPrior <- is.null(priors$sigmaMode) || is.null(priors$sigmaSD)
  if(useUniformPrior) {
    priors$sigmaLo <- sd(y) / 1000
    priors$sigmaHi <- sd(y) * 1000
  } else {
    # Convert to Shape/Rate
    rate <- (priors$sigmaMode +
      sqrt(priors$sigmaMode^2 + 4 * priors$sigmaSD^2)) / (2 * priors$sigmaSD^2)
    shape <- 1 + priors$sigmaMode * rate
    priors$Sh <- shape
    priors$Ra <- rate
  }
  priors$sigmaMode <- NULL
  priors$sigmaSD <- NULL

  # THE MODEL.
  modelFile <- file.path(tempdir(), "BESTmodel.txt")
  if(useUniformPrior) {
    modelString = "
    model {
      for ( i in 1:Ntotal ) {
        y[i] ~ dnorm(mu, tau)
      }
      mu ~ dnorm(muMean, muP)
      tau <- pow(sigma, -2)
      sigma ~ dunif(sigmaLo, sigmaHi)
    }
    " # close quote for modelString
  } else {    # use gamma priors
    modelString = "
    model {
      for ( i in 1:Ntotal ) {
        y[i] ~ dnorm(mu, tau)
      }
      mu ~ dnorm(muMean, muP)
      tau <- pow(sigma, -2)
      sigma ~ dgamma(Sh, Ra)
    }
    " # close quote for modelString
  }
  # Write out modelString to a text file
  writeLines( modelString , con=modelFile )

  # THE DATA.
  # add priors and data to dataForJAGS:
  dataForJAGS <- priors
  if(!doPriorsOnly)
    dataForJAGS$y <- y
  dataForJAGS$Ntotal <- length(y)

  # INTIALIZE THE CHAINS.
  # Initial values of MCMC chains based on data:
  inits <- function() list(mu=mean(y), sigma=sd(y))

  # RUN THE CHAINS
  codaSamples <- justRunJags(dataForJAGS, inits, c("mu", "sigma"), modelFile,
            chains, draws, burnin, thin, adapt,
            modules = c("glm"), parallel = parallel, seed=seed)

  out <- mcmcOutput(codaSamples,
      header = "Model fitted in JAGS with 'rjags' functions")
  attr(out, "call") <- match.call()
  attr(out, "doPriorsOnly") <- doPriorsOnly
  if(!is.null(priors))
    attr(out, "priors") <- priors
  attr(out, "nChains") <- chains
  attr(out, "timeTaken") <- unclass(difftime(Sys.time(), startTime, units="secs"))
  return(out)
}
