##' Identity Matrix Minus Averaging Matrix
##' 
##' Construct a square matrix which the identity matrix minus the averging
##' matrix.
##' 
##' 
##' @param n a numeric describes the dimension of the square matrix.
##' @return This function return a square matrix which the identity matrix
##' minus the averaging matrix.
##' @author Kevin Chang
##' @examples
##' 
##' 
##' J(10)
##' 
##' 
##' @export J
J <- function(n) diag(n) - K(n) 
