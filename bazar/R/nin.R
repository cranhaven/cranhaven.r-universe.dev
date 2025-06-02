#' @title
#' Value matching
#'
#' @description
#' The function \code{\%nin\%} is the negation 
#' of the function \code{\%in\%}.
#'
#' @param x
#' vector or NULL: the values to be matched.
#'
#' @param table
#' vector or NULL: the values to be matched against.
#'
#' @return
#' A logical vector, indicating if a non-match was located for each element of x:
#' thus the values are TRUE or FALSE and never NA.
#'
#' @seealso \code{\link{match}}.
#'
#' @export
#'
#' @examples
#' 1:10 %nin% c(1,3,5,9)
#'
"%nin%" <-
function(x,
         table)
{
  match(x, table, nomatch = 0L) == 0L
}
