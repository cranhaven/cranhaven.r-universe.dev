#' @title Pearson Correlation for Sparse Matrices
#' @description Pearson Correlation for Sparse Matrices.
#' More memory and time-efficient than \code{cor(as.matrix(x))}.
#' @param x A matrix, potentially a sparse matrix such as a "dgCMatrix" object
#' @return a correlation matrix
#' @export
dtmcorr <- function(x) {
  n <- nrow(x)
  covmat <- (as.matrix(Matrix::crossprod(x)) - n * Matrix::tcrossprod(Matrix::colMeans(x))) / (n - 1)
  cormat <- covmat / Matrix::tcrossprod(sqrt(Matrix::diag(covmat)))
  cormat
}