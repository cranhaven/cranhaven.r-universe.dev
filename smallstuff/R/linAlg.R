#########1#########2#########3#########4#########5#########6#########7#########8
#' Cross Product (Linear Algebra)
#'
#' Calculate the cross product as defined in linear algebra; note that this
#' differs from the cross product as defined by R.
#'
#' @param x vector of length 3.
#' @param y vector of length 3.
#' @return Cross product of \code{x} and \code{y}.
#' @examples
#' x=c(1,2,1)
#' y=1:3
#' laCrossProd(x,y)
#' @export
################################################################################
laCrossProd<-function(x,y) {
  if (!inherits(x,c("integer","numeric"))) stop("x must be numeric")
  if (!inherits(y,c("integer","numeric"))) stop("y must be numeric")
  if (length(x)!=3||length(y)!=3) stop("Input vectors must have dimension 3")
  c((x[2]*y[3]-x[3]*y[2]),(x[3]*y[1]-x[1]*y[3]),(x[1]*y[2]-x[2]*y[1]))
}
