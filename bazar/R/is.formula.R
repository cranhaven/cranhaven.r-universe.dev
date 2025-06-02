#' @title 
#' Test if an object is a formula
#' 
#' @description 
#' The function \code{is.formula} tests if the object 
#' \code{x} is a formula. 
#' 
#' @param x
#' An object. 
#' 
#' @return 
#' A logical, \code{TRUE} if \code{x} is a formula. 
#' 
#' @export
#' 
#' @examples 
#' is.formula("this is a formula")
#' is.formula(f <- formula("y ~ x"))
#' is.formula(update(f, ~ . -1))
#' 
is.formula <-
function(x)
{
  typeof(x) == "language" && 
    inherits(x, "formula") && 
    (length(x) %in% c(2L, 3L))
}
