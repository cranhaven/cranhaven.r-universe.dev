#' Reliability coefficients
#'
#' The congeneric reliability and standardized reliability; also the
#'    bias `b` so that `omega` - `alpha` = `b`.
#'
#' @param sigma For `alpha` and `alpha_std`, either apositive definite
#'   covariance matrix or a vector of standard deviations. A vector of
#'   standard deviations for `omega` and `omega_std`.
#' @param lambda Vector of loadings.
#' @return The congeneric reliability standardized reliability,
#'   coefficient alpha, standardized, orsigma coefficient alpha.
#' @name reliability

#' @keywords internal
#' @rdname reliability
alpha_bias <- function(sigma, lambda, w = rep(1, length(lambda))) {
  k <- length(lambda)
  c(k / (k - 1) * (-crossprod(w, lambda)^2 / k + crossprod(w^2, lambda^2)) /
    (crossprod(w, lambda)^2 + crossprod(w^2, sigma^2)))
}

#' @keywords internal
#' @rdname reliability
omega <- function(sigma, lambda) {
  k <- length(lambda)
  a <- k * mean(abs(lambda))^2 / mean(sigma^2)
  a / (a + 1)
}

#' @keywords internal
#' @rdname reliability
omega_std <- function(sigma, lambda) {
  k <- length(lambda)
  a <- k * mean(abs(lambda) / sqrt(lambda^2 + sigma^2))^2 /
    mean(sigma^2 / (lambda^2 + sigma^2))
  a / (a + 1)
}

#' @keywords internal
#' @rdname reliability
alpha <- function(sigma, lambda) {
  if (!missing(lambda)) sigma <- covmat(lambda, sigma)
  k <- nrow(sigma)
  k / (k - 1) * (1 - tr(sigma) / sum(sigma))
}

#' @keywords internal
#' @rdname reliability
alpha_std <- function(sigma, lambda) {
  if (!missing(lambda)) sigma <- covmat(lambda, sigma)
  rho <- stats::cov2cor(sigma)
  k <- nrow(sigma)
  k / (k - 1) * (1 - k / sum(rho))
}
