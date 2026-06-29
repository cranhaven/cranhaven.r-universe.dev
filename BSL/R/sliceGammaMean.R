#' Generate a random sample of gamma for the R-BSL-M method of
#' \insertCite{Frazier2019;textual}{BSL} using slice sampling
#'
#' @description This function updates the gamma of the R-BSL-M method of
#'   \insertCite{Frazier2019;textual}{BSL} with a slice sampler
#'   \insertCite{Neal2003}{BSL}. Note this function is mainly designed for
#'   internal usage.
#'
#' @param loglike The current log synthetic likelihood. This is computed with
#'   function \code{\link{synLikeMisspec}} with the current gamma value.
#' @param tau Scale (or inverse rate) parameter of the Laplace prior
#'   distribution for gamma.
#' @param w Step size for the stepping out in the slice sampler. The default
#'   step size is 1.
#' @param std Standard deviation of the columns of ssx. If this is not
#'   \code{NULL}, it will be used to save computation effort.
#' @param maxit The maximum number of iteration of the stepping out and shrink
#'   steps of slice sampler. The default is 1e3.
#' @inheritParams gaussianSynLike
#' @inheritParams synLikeMisspec
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @seealso \code{\link{sliceGammaVariance}} for the slice sampler of the
#'   variance inflated target distribution.
#'
#' @keywords internal
sliceGammaMean <- function(ssy, ssx, loglike, gamma = numeric(length(ssy)), tau = 1, w = 1,
                           std = NULL, maxit = 1e3) {
  logGammaPrior <- function(x) dLaplace(x, rate = 1 / tau)
  
  mu <- colMeans(ssx)
  Sigma <- cov(ssx)
  if (is.null(std) || length(std) != length(gamma)) {
    std <- apply(ssx, FUN = sd, MARGIN = 2)
  }
  gammaCurr <- gamma
  
  for (i in 1 : length(gamma)) {
    A <- loglike
    B <- logGammaPrior(gammaCurr)
    target <- A + B - rexp(1)
    
    curr <- gammaCurr[i]
    lower <- curr - w
    upper <- curr + w
    
    # step out for lower limit
    iter <- 1
    while (iter <= maxit) {
      gammaLower <- gammaCurr
      gammaLower[i] <- lower
      muLower <- mu + std * gammaLower
      A <- mvtnorm::dmvnorm(ssy, mean = muLower, sigma = Sigma, log = TRUE)
      B <- logGammaPrior(gammaLower)
      targetLower <- A + B
      if (targetLower < target) {
        break
      }
      lower <- lower - w
      iter <- iter + 1
    }
    
    # step out for upper limit
    iter <- 1
    while (iter <= maxit) {
      gammaUpper <- gammaCurr
      gammaUpper[i] <- upper
      muUpper <- mu + std * gammaUpper
      A <- mvtnorm::dmvnorm(ssy, mean = muUpper, sigma = Sigma, log = TRUE)
      B <- logGammaPrior(gammaUpper)
      targetUpper <- A + B
      if (targetUpper < target) {
        break
      }
      upper <- upper + w
      iter <- iter + 1
    }
    
    # shrink
    iter <- 1
    while (iter <= maxit) {
      prop <- runif(1, lower, upper)
      gammaProp <- gammaCurr
      gammaProp[i] <- prop
      muProp <- mu + std * gammaProp
      A <- mvtnorm::dmvnorm(ssy, mean = muProp, sigma = Sigma, log = TRUE)
      B <- logGammaPrior(gammaProp)
      targetProp <- A + B
      if (targetProp > target) {
        gammaCurr <- gammaProp
        loglike <- A
        break
      }
      if (prop < curr) {
        lower <- prop
      } else {
        upper <- prop
      }
      iter <- iter + 1
    }
  }
  
  ret <- gammaCurr
  attr(ret, 'loglike') <- loglike
  return (ret)
}

dLaplace <- function(x, rate = 1) {
  n <- length(x)
  n * log(rate / 2) - rate * sum(abs(x))
}
