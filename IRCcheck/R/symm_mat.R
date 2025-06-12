#' @title  Symmetric Matrix
#' 
#' Copy the upper triangular of a matrix into the lower triangular portion of the matrix.
#' 
#' @param x A matrix of dimensions \emph{p} by \emph{p}.
#'
#' @return A matrix
#' @export
#'
#' @examples
#' adj <- matrix(sample(0:1, size = 25, replace = TRUE), 5, 5)
#' symm_mat(adj)
#' 
symm_mat <- function (x) {
  x[lower.tri(x)] <- t(x)[lower.tri(x)]
  x
}