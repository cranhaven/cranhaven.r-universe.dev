#' Thurstone weights
#'
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @return The Thurstone weights.
#' @keywords internal
thurstone <- function(lambda, sigma) {
  c(lambda / (sigma^2 * (1 + sum(lambda^2 / sigma^2))))
}

#' Trace of matrix
#' @param mat A square matrix.
#' @return Trace of the matrix.
#' @keywords internal
tr <- function(mat) sum(diag(mat))

#' Transform lambda and sigma to a covariance matrix.
#' @param lambda Vector of loadings.
#' @param sigma Vector of standard deviations.
#' @keywords internal
#' @return The covariance matrix implied by lambda and sigma.
covmat <- function(lambda, sigma) diag(sigma^2) + lambda %*% t(lambda)
