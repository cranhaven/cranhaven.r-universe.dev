#' Random number generation for the MSEN distribution
#'
#' @param n An integer specifying the number of data points to be simulated.
#' @param mu A vector of length \code{d}, where \code{d} is the dimensionality, representing the mean value.
#' @param Sigma A symmetric positive-definite matrix representing the scale matrix of the distribution.
#' @param theta A number greater than 0 indicating the tailedness parameter.
#'
#' @return A list with the following elements:
#' \item{X}{A data matrix with \code{n} rows and \code{d} columns.}
#' \item{w}{A vector of weights of dimension \code{n}.}
#' @export
#' @references
#' Punzo A., and Bagnato L. (2020). Allometric analysis using the multivariate shifted exponential normal distribution.
#' *Biometrical Journal*, **62**(6), 1525-1543.
#'
#' @examples
#' d <- 3
#' rmsen(10, mu = rep(0, d), Sigma = diag(d), theta = 0.3)
rmsen <- function(n, mu = rep(0, d), Sigma, theta = Inf) {
  if (missing(Sigma)) {
    stop("Sigma is missing")
  }
  if (theta < 0) {
    stop("theta must be greater than, or equal to, 0")
  }
  if (is.matrix(Sigma)) {
    d <- ncol(Sigma)
  }
  if (!is.matrix(Sigma)) {
    d <- 1
  }
  X <- array(0, c(n, d), dimnames = list(1:n, paste("X.", 1:d, sep = "")))
  w <- (1 + stats::rexp(n = n, theta))

  for (i in 1:n) X[i, ] <- rmnorm(n = 1, mean = mu, varcov = Sigma / w[i])

  return(list(X = X, w = w))
}
