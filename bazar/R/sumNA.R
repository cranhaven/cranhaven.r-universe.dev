#' @title
#' Modified sum of vector elements 
#'
#' @description
#' The function \code{sumNA} returns the sum of all the values in its arguments.
#' Contrarily to \code{\link{sum}}, it returns \code{NA} instead of \code{0} when the input 
#' contains only missing values and missing values are removed. 
#'  
#' @param ...
#' numeric or complex or logical vectors.  
#'
#' @param na.rm
#' logical. Should missing values (including \code{NaN}) be removed? 
#'
#' @return 
#' The sum. Returns \code{NA} if \code{x} contains only missing values 
#' and \code{na.rm = TRUE}. 
#'
#' @seealso \code{\link{sum}}.
#'
#' @export
#'
#' @examples
#' x <- c(NA, NA)
#' sum(x)
#' sumNA(x)
#' sum(x, na.rm = TRUE)
#' sumNA(x, na.rm = TRUE) # here is the difference with 'sum()'
#' 
#' sum(c())
#' sumNA(c())
#' 
sumNA <-
function(..., 
         na.rm = FALSE)
{
  x <- unlist(list(...))
  if (na.rm && length(x) && all(is.na(x))) return(x[1] + NA)
  sum(x, na.rm = na.rm)
}
