#' computeGridLambda
#'
#' Construct the data-driven grid for the regularization parameters used for the Lasso estimator
#'
#' @param phiInit value for phi
#' @param rhoInit  for rho
#' @param piInit  for pi
#' @param gamInit value for gamma
#' @param X matrix of covariates (of size n*p)
#' @param Y matrix of responses (of size n*m)
#' @param gamma power of weights in the penalty
#' @param mini minimum number of iterations in EM algorithm
#' @param maxi maximum number of iterations in EM algorithm
#' @param eps threshold to stop EM algorithm
#' @param fast boolean to enable or not the C function call
#'
#' @return the grid of regularization parameters for the Lasso estimator. The output is a vector with nonnegative values that are relevant
#' to be considered as regularization parameter as they are equivalent to a 0 in the regression parameter.
#'
#' @export
computeGridLambda <- function(phiInit, rhoInit, piInit, gamInit, X, Y, gamma, mini,
  maxi, eps, fast)
{
  n <- nrow(X)
  p <- ncol(X)
  m <- ncol(Y)
  k <- length(piInit)

  list_EMG <- EMGLLF(phiInit, rhoInit, piInit, gamInit, mini, maxi, gamma, lambda = 0,
    X, Y, eps, fast)

  grid <- array(0, dim = c(p, m, k))
  for (j in 1:p)
  {
    for (mm in 1:m)
      grid[j, mm, ] <- abs(list_EMG$S[j, mm, ])/(n * list_EMG$pi^gamma)
  }
  sort(unique(grid))
}
