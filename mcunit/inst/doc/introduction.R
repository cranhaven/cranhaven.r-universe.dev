## -----------------------------------------------------------------------------
require(mcunit)
set.seed(10)

## -----------------------------------------------------------------------------
gibbsUpdate <- function(y, theta_j) {
  # Samples theta_i given y and theta_j
  mean <- 100 * (y - theta_j) / (100 + 0.1)
  var <- 1. / (1. / 100 + 1. / 0.1)
  rnorm(1, mean=mean, sd=sqrt(var))
}

## -----------------------------------------------------------------------------
randomScan <- function(theta, y, thinning) {
  # Random Scan Gibbs
  for(i in 1:thinning) {
    # select index to update
    i <- sample.int(2,1)
    theta[i] <- gibbsUpdate(y, theta[i %% 2 + 1])
  }
  theta
}

## -----------------------------------------------------------------------------
obj <- list()
obj$genprior <- function() rnorm(n=2, mean=0, sd=10)
obj$gendata <- function(theta) sum(theta) + rnorm(n=1, mean=0, sd=sqrt(0.1))

## -----------------------------------------------------------------------------
priorDensity <- function(theta) prod(dnorm(theta, mean=0, sd=10))
likelihood <- function(theta, y) dnorm(y, mean=sum(theta), sd=sqrt(0.1))
testVec <- function(theta, y) c(theta[1], theta[2], priorDensity(theta), likelihood(theta, y)) 
obj$test <- testVec

## -----------------------------------------------------------------------------
obj$stepMCMC <- function(theta, dat, thinning) randomScan(theta, dat, thinning)
print(obj)

## -----------------------------------------------------------------------------
expect_mcmc(obj, thinning = 100)

## -----------------------------------------------------------------------------
expect_mcmc_reversible(obj, thinning = 10, nsteps = 10)

## -----------------------------------------------------------------------------
systematicScan <- function(theta, y, thinning) {
  # Systematic Scan Gibbs
  for(i in 1:thinning) {
    theta[1] <- gibbsUpdate(y, theta[2])
    theta[2] <- gibbsUpdate(y, theta[1])
  }
  theta
}

## -----------------------------------------------------------------------------
obj$stepMCMC <- function(theta, dat, thinning) systematicScan(theta, dat, thinning)
expect_mcmc(obj, thinning = 100)

## -----------------------------------------------------------------------------
gibbsUpdate <- function(y, theta_j) {
  # Samples theta_i given y and theta_j
  mean <- 100 * (y - theta_j) / (100 + 0.1)
  var <- 1. / (1. / 10 + 1. / sqrt(0.1))
  rnorm(1, mean=mean, sd=sqrt(var))
}

## -----------------------------------------------------------------------------
obj$stepMCMC <- function(theta, dat, thinning) randomScan(theta, dat, thinning)

