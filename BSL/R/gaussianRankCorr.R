#' Gaussian rank correlation
#'
#' @description This function computes the Gaussian rank correlation of
#'   \insertCite{Boudt2012;textual}{BSL}.
#' @param x         A numeric matrix representing data where the number of rows
#'   is the number of independent data points and the number of columns is the
#'   number of variables in the dataset.
#' @param vec      A logical argument indicating if the vector of correlations
#'   should be returned instead of a matrix.
#' @return         Gaussian rank correlation matrix (default) or a vector of
#'   pair correlations.
#' @references
#'
#' \insertAllCited{}
#'
#' @rdname gaussianRankCorr
#' @examples
#' data(ma2)
#' model <- newModel(fnSimVec = ma2_sim_vec, fnSum = ma2_sum, simArgs = list(TT = 10),
#'                   theta0 = ma2$start, fnLogPrior = ma2_prior)
#' set.seed(100)
#'
#' # generate 1000 simualtions from the ma2 model
#' x <- simulation(model, n = 1000, theta = c(0.6, 0.2))$x
#'
#' corr1 <- cor(x) # traditional correlation matrix
#' corr2 <- gaussianRankCorr(x) # Gaussian rank correlation matrix
#' oldpar <- par()
#' par(mfrow = c(1, 2))
#' image(corr1, main = 'traditional correlation matrix')
#' image(corr2, main = 'Gaussian rank correlation matrix')
#' par(mfrow = oldpar$mfrow)
#'
#' std <- apply(x, MARGIN = 2, FUN = sd) # standard deviations
#' cor2cov(gaussianRankCorr(x), std) # convert to covariance matrix
#'
#' @seealso       \code{\link{cor2cov}} for conversion from correlation matrix
#'   to covariance matrix.
#' @export
gaussianRankCorr <- function(x, vec = FALSE) {
  n <- nrow(x)
  p <- ncol(x)
  stopifnot(p >= 2)
  r <- apply(x, FUN = rank, MARGIN = 2, ties.method = "average")
  rqnorm <- qnorm(r / (n + 1))
  den <- sum((qnorm(1 : n / (n + 1))) ^ 2)
  res <- unlist(sapply(1:(p-1), FUN = function(i) c(rqnorm[, i] %*% rqnorm[, (i+1):p]))) / den
  if (!vec) {
    res <- p2P(res)
  }
  return (res)
}

#' Convert a correlation matrix to a covariance matrix
#' @description This function converts a correlation matrix to a covariance matrix
#' @param corr    The correlation matrix to be converted. This must be symmetric.
#' @param std     A vector that contains the standard deviations of the variables in the correlation matrix.
#' @return        The covariance matrix.
#' @export
cor2cov <- function(corr, std) {
  outer(std, std) * corr
}
