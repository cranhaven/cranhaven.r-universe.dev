#' @title 
#' Normalize a numeric vector
#' 
#' @description 
#' This function divides \code{x} by the result of \code{fun(x)}. 
#' 
#' @param x
#' numeric. A vector. 
#' 
#' @param fun
#' character or function. Should own an \code{na.rm} argument. \code{fun(x)} 
#' should return either one unique value, or a numeric vector of the same length 
#' as \code{x}. 
#' 
#' @param na.rm 
#' Should missing values be removed in the calculation of \code{fun(x)}? 
#' 
#' @param ...
#' Additional arguments to be passed to \code{fun}. 
#' 
#' @return 
#' A numeric vector of the same length as \code{x}. 
#' 
#' @export
#' 
#' @examples 
#' x <- rnorm(10)
#' normalize(x)
#' 
normalize <- 
function(x, 
         fun = "max", 
         na.rm = TRUE, 
         ...)
{
  fun <- as.fun(fun)
  x/fun(x, na.rm = na.rm, ...)
}
