#' @title
#' Almost unique elements
#'
#' @description
#' The function \code{almost.unique} extracts elements of a vector \code{x} 
#' that are unique up to a tolerance factor.
#'
#' @param x
#' numeric. The vector of numeric values at stake.
#'
#' @param tolerance
#' numeric. Relative differences smaller than tolerance are considered as equal.  
#' The default value is close to \code{1.5e-8}. 
#' 
#' @param ...
#' Additional arguments to be passed to the function 
#' \code{\link[base]{duplicated}}, which is used internally by 
#' \code{almost.unique}. 
#'
#' @return
#' A vector of the same type as \code{x}.
#' 
#' @seealso 
#' \code{\link[base]{unique}}, 
#' \code{\link[base]{duplicated}}.
#' 
#' @export
#' 
#' @examples 
#' almost.unique(c(1, 1.01), tol = 0.1)
#' almost.unique(c(1, 1.01), tol = 0.01)
#' 
#' almost.unique(c(1, 2, 3), tol = 10)
#' almost.unique(c(1, 2, 3), tol = 5)
#' almost.unique(c(1, 2, 3), tol = 1)
#' 
almost.unique <- 
function(x, 
         ...)
{
  UseMethod("almost.unique")
}


#' @export
#' @rdname almost.unique
#' 
almost.unique.default <- 
function(x, 
         tolerance = sqrt(.Machine$double.eps),
         ...)
{
  y <- round(x/tolerance, 0)
  d <- duplicated(y, ...)
  x[!d]
}
