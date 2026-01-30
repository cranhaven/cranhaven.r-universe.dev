#########1#########2#########3#########4#########5#########6#########7#########8
#' Create the Projection Matrix of a Matrix
#'
#' Calculates the projection matrix for a full-rank matrix X with its
#' number of rows greater than or equal to its number of columns
#'
#' @param X nxp Matrix; must be full-rank and have n >= p
#' @return Projection matrix of \code{X}.
#' @examples
#' projMatrix(matrix(c(3,4,-1,2,1,1),3))
#' @export
################################################################################
projMatrix<-function(X) {
  X=as.matrix(X)
  if (ncol(X) > nrow(X)) stop("Matrix has more columns than rows")
  if (Matrix::rankMatrix(X)!=ncol(X)) stop("Matrix is not full rank")
  X%*%(solve(t(X)%*%X))%*%t(X)
}
