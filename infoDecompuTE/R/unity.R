# Unity matrix


##' Construct a unity vector
##' 
##' Construct a vector with all elements unity.
##' 
##' 
##' @param n a numeric describe the length of vector.
##' @return This function returns a \eqn{n \\times 1} matrix will all elements
##' unity.
##' @author Kevin Chang
##' @references John J, Williams E (1987). \emph{Cyclic and computer generated
##' Designs}. Second edition. Chapman & Hall.
##' @examples
##' 
##' 
##' unity(10)
##' 
##' 
##' @export unity
unity <- function(n) matrix(rep(1, n), nrow = n, ncol = 1) 
