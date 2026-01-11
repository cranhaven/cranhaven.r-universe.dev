# Trace operation



##' Trace of the Matrix
##' 
##' Compute the trace of the square matrix.
##' 
##' 
##' @param X a square matrix.
##' @return A numeric value.
##' @author Kevin
##' @seealso \code{\link{diag}}
##' @references John J, Williams E (1987). \emph{Cyclic and computer generated
##' Designs}. Second edition. Chapman & Hall.
##' @examples
##' 
##' 
##' m = matrix(1, nrow = 10, ncol = 10)
##' tr(m)   
##' 
##' 
##' @export tr
tr <- function(X) sum(diag(X)) 
