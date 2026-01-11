##' Identity Matrix
##' 
##' Construct an identity matrix.
##' 
##' 
##' @param n a numeric describes the dimension of the identity matrix.
##' @return This function returns a matrix with the diagonal elements equal to
##' one and the off-diagonal elements equal to zero.
##' @author Kevin
##' @seealso \code{\link{diag}}
##' @references John J, Williams E (1987). \emph{Cyclic and computer generated
##' Designs}. Second edition. Chapman & Hall.
##' @examples
##' 
##' 
##' identityMat(10)
##' 
##' 
##' @export identityMat
identityMat <- function(n) diag(n) 
