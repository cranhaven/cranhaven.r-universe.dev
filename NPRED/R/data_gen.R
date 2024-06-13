#' Generate predictor and response data.
#'
#' @param nobs The data length to be generated.
#' @param ndim The number of potential predictors (default is 9).
#'
#' @return A list of 2 elements: a vector of response (x), and a matrix of potential predictors (dp) with each column containing one potential predictor.
#' @export
#'
#' @examples
#' \donttest{
#' # AR1 model from paper with 9 dummy variables
#' data.ar1 <- data.gen.ar1(500)
#' stepwise.PIC(data.ar1$x, data.ar1$dp)
#' }
data.gen.ar1 <- function(nobs, ndim = 9) {
  nwarm1 <- nwarm2 <- 30
  n <- nobs + nwarm1 + nwarm2
  x <- matrix(0, n, 1)
  for (i in 1:nwarm1) {
    x[i] <- rnorm(1, mean = 0, sd = 1)
  }
  dp <- matrix(0, (nobs), ndim)
  for (i in (nwarm1 + 1):n) {
    eps <- rnorm(1, mean = 0, sd = 1)
    x[i] <- 0.9 * x[i - 1] + 0.866 * eps
  }
  for (i in 1:ndim) dp[, i] <- x[(n - i - nobs + 1):(n - i)]
  x <- x[(n - nobs + 1):n]
  data_generated <- list(x = x, dp = dp)
  return(data_generated)
}

#' Generate predictor and response data.
#'
#' @param nobs The data length to be generated.
#' @param ndim The number of potential predictors (default is 9).
#'
#' @return A list of 2 elements: a vector of response (x), and a matrix of potential predictors (dp) with each column containing one potential predictor.
#' @export
#'
#' @examples
#' \donttest{
#' # AR4 model from paper with total 9 dimensions
#' data.ar4 <- data.gen.ar4(500)
#' stepwise.PIC(data.ar4$x, data.ar4$dp)
#' }
data.gen.ar4 <- function(nobs, ndim = 9) {
  nwarm1 <- nwarm2 <- 30
  n <- nobs + nwarm1 + nwarm2
  x <- matrix(0, n, 1)
  for (i in 1:nwarm1) {
    x[i] <- rnorm(1, mean = 0, sd = 1)
  }
  dp <- matrix(0, (nobs), ndim)
  for (i in (nwarm1 + 1):n) {
    eps <- rnorm(1, mean = 0, sd = 1)
    x[i] <- 0.6 * x[i - 1] - 0.4 * x[i - 4] + eps
  }
  for (i in 1:ndim) dp[, i] <- x[(n - i - nobs + 1):(n - i)]
  x <- x[(n - nobs + 1):n]
  data_generated <- list(x = x, dp = dp)
  return(data_generated)
}

#' Generate predictor and response data.
#'
#' @param nobs The data length to be generated.
#' @param ndim The number of potential predictors (default is 9).
#'
#' @return A list of 2 elements: a vector of response (x), and a matrix of potential predictors (dp) with each column containing one potential predictor.
#' @export
#'
#' @examples
#' \donttest{
#' # AR9 model from paper with total 9 dimensions
#' data.ar9 <- data.gen.ar9(500)
#' stepwise.PIC(data.ar9$x, data.ar9$dp)
#' }
data.gen.ar9 <- function(nobs, ndim = 9) {
  nwarm1 <- nwarm2 <- 30
  n <- nobs + nwarm1 + nwarm2
  x <- matrix(0, n, 1)
  for (i in 1:nwarm1) {
    x[i] <- rnorm(1, mean = 0, sd = 1)
  }
  dp <- matrix(0, (nobs), ndim)
  for (i in (nwarm1 + 1):n) {
    eps <- rnorm(1, mean = 0, sd = 1)
    x[i] <- 0.3 * x[i - 1] - 0.6 * x[i - 4] - 0.5 * x[i - 9] + eps
  }
  for (i in 1:ndim) dp[, i] <- x[(n - i - nobs + 1):(n - i)]
  x <- x[(n - nobs + 1):n]
  data_generated <- list(x = x, dp = dp)
  return(data_generated)
}
