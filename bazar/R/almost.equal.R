#' @title 
#' Test (almost) equality of numeric values
#' 
#' @description 
#' The function \code{almost.equal} tests if two numeric vectors 
#' have equal values up to a tolerance. 
#' 
#' @param x
#' numeric vector. 
#' 
#' @param y
#' numeric vector of the same length as \code{x}. 
#' 
#' @param tolerance 
#' numeric. Differences smaller than tolerance are considered as equal.  
#' The default value is close to \code{1.5e-8}. 
#' 
#' @return
#' A logical vector of the same length as \code{x} and \code{y}. 
#' 
#' @author 
#' Tommy on StackOverflow, see \url{http://stackoverflow.com/a/7667703}. 
#' 
#' @export
#' 
#' @examples 
#' almost.equal(x = 1:3, 
#'              y = 1:3 + c(10^(-6), 10^(-7), 10^(-8)))
#' 
almost.equal <- 
function(x, 
         y, 
         tolerance = sqrt(.Machine$double.eps))
{
  diff <- abs(x - y)
  mag <- pmax(abs(x), abs(y))
  ifelse(mag > tolerance, diff/mag <= tolerance, diff <= tolerance)
}
