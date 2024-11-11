#' Fast Entropy Estimation of Multi-Dimensional Data
#'
#' Non-parametric estimator for the differential entropy of a multidimensional distribution, given a limited set of data points, by a recursive rectilinear partitioning.
#'
#' @param X \[\code{matrix}\]\cr
#'          Data, one observation per row.
#' @param ci \[\code{numeric(1)}\]\cr
#'          Confidence threshold used to decide if a cell should be divided further. 
#'          Defaults to 95%.
#' @param lower \[\code{numeric(n)}\]\cr
#'        Lower bound of the support of \code{X}.
#' @param upper \[\code{numeric(n)}\]\cr
#'        Upper bound of the support of \code{X}.
#'
#' @return Differential entropy estimate.
#'
#' @references
#' D. Stowell and M. D. Plumbley
#' Fast multidimensional entropy estimation by k-d partitioning.
#' IEEE Signal Processing Letters 16 (6), 537--540, June 2009.
#' http://dx.doi.org/10.1109/LSP.2009.2017346
#'
#' @examples
#' Xu <- matrix(runif(1000 * 100), ncol=100)
#' kdpee(Xu)
#'
#' Xn <- matrix(rnorm(1000 * 100), ncol=100)
#' kdpee(Xn)
#'
#' @importFrom checkmate assertMatrix
#' @importFrom checkmate assertNumber
#' @importFrom checkmate assertNumeric
#' @importFrom stats qnorm
#' @export
kdpee <- function(X, ci = 0.95, lower = apply(X, 2, min), upper = apply(X, 2, max)) {
  assertMatrix(X, mode = 'numeric', min.cols = 2L, any.missing = FALSE)
  assertNumber(ci, lower = 0, upper = 1, finite = TRUE)
  assertNumeric(lower, min.len = 1, max.len = ncol(X), any.missing = FALSE)
  assertNumeric(upper, min.len = 1, max.len = ncol(X), any.missing = FALSE)

  if (length(lower) < ncol(X))
    lower <- rep_len(lower, ncol(X))
  if (length(upper) < ncol(X))
    upper <- rep_len(upper, ncol(X))

  z <- abs(qnorm((1 - ci)/2))
  .Call(do_kdpee, X, z, lower, upper)
}
