# projection matrix






##' Construct a Projection Matrix
##' 
##' Compute the projection matrix from a square matrix.
##' 
##' 
##' @param X a square matrix.
##' @return A square matrix.
##' @author Kevin Chang
##' @examples
##' 
##' 
##' m = matrix(1, nrow = 10, ncol = 3)
##' projMat(m) 
##' 
##' 
##' @export projMat
projMat <- function(X) X %*% ginv(t(X) %*% X) %*% t(X) 
