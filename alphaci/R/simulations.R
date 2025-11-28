#' Simulate observations from a congeneric one-factor model.
#'
#' Simulate observations from a one-factor model with specified latent variable
#'    and error variable distributions. The error terms are not correlated,
#'    hence the model is congeneric.
#'
#' @param n Number of observations.
#' @param k Size of matrix or number of testlets.
#' @param sigma Vector of error standard deviations.
#'     Repeated to have `k` elements.
#' @param lambda Vector of factor loadings. Repeated to have `k` elements.
#' @param latent Distribution of the latent variable.
#' @param error Distribution of the error variable.
#' @return Covariance matrix.
#' @keywords internal
simulate_congeneric <- function(n,
                                k,
                                sigma = 1,
                                lambda = 1,
                                latent = stats::rnorm, error = stats::rnorm) {
  lambda <- rep(lambda, length.out = k)
  sigma <- rep(sigma, length.out = k)
  eps <- matrix(error(k * n), nrow = n)
  mat <- matrix(rep(latent(n), k), nrow = n)
  sweep(mat, 2, lambda, "*") + sweep(eps, 2, sigma, "*")
}
