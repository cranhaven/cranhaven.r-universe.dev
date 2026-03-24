#' Compute upper triangle Jaccard distance
#'
#' @param mat A numeric binary matrix (0/1/NA)
#' @param as.dist Logical. If TRUE, return a dist object; otherwise an upper triangular matrix.
#' @return A matrix or dist object of Jaccard dissimilarities (upper triangle)
#' @importFrom stats as.dist
#' @import RcppParallel
#' @importFrom Matrix forceSymmetric
#' @export
fast_jaccard_dist <- function(mat, as.dist = FALSE) {
  #res <- .Call('_MSCA_jaccard_index_rcpp_upper', mat)
  res <- .Call('_MSCA_jaccard_index_rcpp_upper', mat)
  res[is.na(res)] <- 1
  res <- Matrix::forceSymmetric(Matrix::Matrix(res, sparse = FALSE), uplo = "U")
  coln <- colnames(mat)
  dimnames(res) <- list(coln, coln)
  if (as.dist) return(as.dist(res))
  res
}

#' @examples
#' \dontrun{
#' mat <- make_state_matrix(EHR)
#' dim(mat)
#' d1 <- dist(t(mat[, 1:10]), method = 'binary')
#' d2 <- fast_jack_upper(mat)
#' }
