#' Random number generation for the MTIN distribution
#'
#' @param n An integer specifying the number of data points to be simulated.
#' @param mu A vector of length \code{d}, where \code{d} is the dimensionality, representing the mean value.
#' @param Sigma A symmetric positive-definite matrix representing the scale matrix of the distribution.
#' @param theta A number between 0 and 1 indicating the tailedness parameter.
#'
#' @return A list with the following elements:
#' \item{X}{A data matrix with \code{n} rows and \code{d} columns.}
#' \item{w}{A vector of weights of dimension \code{n}.}
#' @export
#' @references
#' Punzo A., and Bagnato L. (2021). The multivariate tail-inflated normal distribution and its application in finance.
#' *Journal of Statistical Computation and Simulation*, **91**(1), 1-36.
#'
#' @examples
#' d <- 3
#' rmtin(10, mu = rep(0, d), Sigma = diag(d), theta = 0.9)
rmtin <- function(n, mu = rep(0, d), Sigma, theta = 0.01) {
  if (missing(Sigma)) {
    stop("Sigma is missing")
  }
  if (theta <= 0 | theta >= 1) {
    stop("theta must be in the interval (0,1)")
  }
  if (is.matrix(Sigma)) {
    d <- ncol(Sigma)
  }
  if (!is.matrix(Sigma)) {
    d <- 1
  }
  X <- array(0, c(n, d), dimnames = list(1:n, paste("X.", 1:d, sep = "")))
  w <- stats::runif(n = n, min = 1 - theta, 1)

  for (i in 1:n) X[i, ] <- rmnorm(n = 1, mean = mu, varcov = Sigma / w[i])

  return(list(X = X, w = w))
}
