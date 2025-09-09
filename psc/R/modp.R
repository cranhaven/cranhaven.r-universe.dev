#' modp
#'
#' A function which rrturns either the input value (if positive) or zero (if
#' negative)
#'
#' @param x a numberic vector
#' @return a numeric vector with negative values replaced with 0
#' @details A fucntion which returns a version of x with negative values replacd with 0
modp <- function(x){
  x*(sign(x)+1)/2
}
