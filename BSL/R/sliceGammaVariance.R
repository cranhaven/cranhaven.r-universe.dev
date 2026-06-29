#' Generate a random sample of gamma for the R-BSL-V method of
#' \insertCite{Frazier2019;textual}{BSL} using slice sampling
#'
#' @description This function updates the gamma parameter with a slice sampler
#'   \insertCite{Neal2003}{BSL}. The target distribution is the variance
#'   inflated approximate posterior of BSL with model misspecification. See
#'   \insertCite{Frazier2019;textual}{BSL}. Note this function is mainly
#'   designed for internal usage.
#'
#' @param tau Numeric. Scale (or inverse rate) parameter of the exponential
#'   prior distribution.
#' @inheritParams gaussianSynLike
#' @inheritParams synLikeMisspec
#' @inheritParams sliceGammaMean
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @seealso \code{\link{sliceGammaMean}} for the slice sampler of the mean
#'   adjusted target distribution.
#'
#' @keywords internal
sliceGammaVariance <- function(ssy, ssx, loglike, gamma = numeric(length(ssy)), tau = 1, w = 1,
                               std = NULL, maxit = 1e3) {
  logGammaPrior <- function(x) sum(dexp(x, rate = 1 / tau, log = TRUE))
  
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
    lower <- 0
    upper <- curr + w
    
    # step out for upper limit
    iter <- -1
    while (iter <= maxit) {
      gammaUpper <- gammaCurr
      gammaUpper[i] <- upper
      SigmaUpper <- Sigma + diag((std * gammaUpper) ^ 2)
      A <- mvtnorm::dmvnorm(ssy, mean = mu, sigma = SigmaUpper, log = TRUE)
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
      SigmaProp <- Sigma + diag((std * gammaProp) ^ 2)
      A <- mvtnorm::dmvnorm(ssy, mean = mu, sigma = SigmaProp, log = TRUE)
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
