#########1#########2#########3#########4#########5#########6#########7#########8
#' Determine if the Input contains Integers
#'
#' Determine if numbers in a vector are integers (not just of integer type)
#'
#' @param x integer or numeric type vector
#' @param inf logical field answering whether an infinite value should be 
#'            considered an integer (default TRUE)
#' @return TRUE for each value in \code{x} that is an integer, FALSE otherwise
#' @examples
#' isInt(c(3,3.23,Inf))
#' @export
################################################################################
isInt<-function(x,inf=TRUE) {
  if (inherits(x,"integer")) return(TRUE)
  if (!inherits(x,"numeric")) return(FALSE)
  if (!inherits(inf,"logical")) stop("inf must be logical")
  out=rep(TRUE,length(x))
  out[is.infinite(x)]=inf[1]
  out[is.finite(x)]=(x[is.finite(x)]-floor(x[is.finite(x)]))==0
  out
}
