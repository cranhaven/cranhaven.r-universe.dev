#'  Get order along the Hilbert curve
#'
#' @param X matrix of values. Observations are unique by rows.
#' @param Sigma Covariance of the data. If provided, uses a Mahalanobis distance.
#'
#' @return Index of orders
#' @export
#'
#' @examples
#' X <- matrix(rnorm(10*3), 3, 10)
#' idx <- hilbert.projection(X)
#' print(idx)
hilbert.projection <- function(X, Sigma = NULL) {
  X <- as.matrix(t(X))
  if(!is.null(Sigma)) {
    X <- backsolve(chol(Sigma), X, transpose = TRUE)
  }
  idx <- hilbert_proj_(X) + 1 # +1 to adjust C to R indexing
  return(idx)
}