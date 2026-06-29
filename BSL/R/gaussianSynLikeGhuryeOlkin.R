#' Estimate the Gaussian synthetic (log) likelihood with an unbiased estimator
#'
#' @description This function computes an unbiased, nonnegative estimate of a
#'   normal density function from simulations assumed to be drawn from it. See
#'   \insertCite{Price2018;textual}{BSL} and
#'   \insertCite{Ghurye1969;textual}{BSL}.
#'
#' @inheritParams gaussianSynLike
#'
#' @return             The estimated synthetic (log) likelihood value.
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @examples
#' data(ma2)
#' ssy <- ma2_sum(ma2$data)
#' m <- newModel(fnSim = ma2_sim, fnSum = ma2_sum, simArgs = ma2$sim_args,
#'               theta0 = ma2$start)
#' ssx <- simulation(m, n = 300, theta = c(0.6, 0.2), seed = 10)$ssx
#'
#' # unbiased estimate of the Gaussian synthetic likelihood
#' # (the likelihood estimator used in uBSL)
#' gaussianSynLikeGhuryeOlkin(ssy, ssx)
#'
#' @seealso    Other available synthetic likelihood estimators:
#'   \code{\link{gaussianSynLike}} for the standard synthetic likelihood
#'   estimator, \code{\link{semiparaKernelEstimate}} for the semi-parametric
#'   likelihood estimator, \code{\link{synLikeMisspec}} for the Gaussian
#'   synthetic likelihood estimator for model misspecification.
#'
#' @export
gaussianSynLikeGhuryeOlkin <- function(ssy, ssx, log = TRUE, verbose = FALSE) {
  d <- length(ssy)
  n <- nrow(ssx)
  mu <- colMeans(ssx)
  Sigma <- cov(ssx)
  psi <- (n-1) * Sigma - (ssy-mu) %*% t(ssy-mu) / (1-1/n)
  
  temp <- try(chol(psi))
  if (inherits(temp, 'try-error')) {
    if (verbose) {
      cat('*** reject (cov(ssx) is not positive definite) ***\n')
    }
    loglike <- -Inf
  } else {
    A <- wcon(d, n-2) - wcon(d, n-1) - 0.5*d*log(1-1/n)
    B <- -0.5 * (n-d-2) * (log(n-1) + logdet(Sigma))
    C <- 0.5 * (n-d-3) * logdet(psi)
    loglike <- -0.5*d*log(2*pi) + A + B + C
  }
  if (!log) {
    loglike <- exp(loglike)
  }
  
  return (loglike)
}

# log of c(k,nu) from Ghurye & Olkin (1969)
wcon <- function(k, nu) {
  cc <- -k*nu/2*log(2) - k*(k-1)/4*log(pi) - sum(lgamma(0.5*(nu-(1:k)+1)))
  cc
}

# calculating the log of the determinant
logdet <- function(A) {
  L <- chol(A)
  y <- 2 * sum(log(diag(L)))
  y
}
