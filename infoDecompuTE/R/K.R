# Averaging matrix






##' Averaging Matrix
##' 
##' Construct a n-by-n averaging matrix.
##' 
##' 
##' @param n a numeric describes the dimension of the averaging matrix.
##' @return This function returns a \eqn{n \times n} square matrix with all
##' elements equal 1/n.
##' @author Kevin Chang
##' @references John J, Williams E (1987). \emph{Cyclic and computer generated
##' Designs}. Second edition. Chapman & Hall.
##' @examples
##' 
##' 
##' K(10)
##' 
##' 
##' @export K
K <- function(n) matrix(1/n, nrow = n, ncol = n) 
