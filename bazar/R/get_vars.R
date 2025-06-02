#' @title 
#' Get formula variables
#' 
#' @description 
#' The function \code{get_vars} extracts variable names from a formula. 
#' 
#' @param  formula
#' a formula. 
#' 
#' @param data
#' data.frame or matrix. If not \code{NULL}, formulas with a dot \code{.} 
#' are permitted. 
#' 
#' @param intersection
#' logical. If \code{TRUE} and \code{data} is not \code{NULL}, the intersection 
#' between variables found in the formula and \code{data} column names is 
#' returned. 
#' 
#' @return 
#' a character vector, the variables found. 
#' 
#' @importFrom stats as.formula setNames terms
#' @export
#' 
#' @seealso 
#' \code{\link{all.vars}}, 
#' \code{\link[formula.tools]{get.vars}}
#' 
#' @examples 
#' get_vars(y ~ x1 + x2 - x1)
#' get_vars(y ~ . - x1, data = data.frame(y = 1, x1 = 2, x2 = 3))
#' get_vars(y + z ~ x1 + x2 - x1 | x3)
#' get_vars(y ~ x1 + I(log(x2)))
#' get_vars(y ~ x1*x2)
#' get_vars(y ~ x1:x2)
#' get_vars(~ x1 + x2)
#' 
get_vars <- 
function(formula, 
         data = NULL, 
         intersection = TRUE)
{
  stopifnot(is.formula(formula))
  if (is.null(data)) {
    # do nothing
  } else if (is.character(data)) {
    data = stats::setNames(1:length(data), data)
    data = as.data.frame(t(data))
  } else if (is.matrix(data)) {
    data = as.data.frame(data)
  } else {
    stopifnot(is.data.frame(data))
  }
  
  Y = labels(stats::terms(formula[-3L], data = data))
  if (length(formula)==3L) {
    X = labels(stats::terms(formula[-2L], data = data))
  } else {
    X = character(0L)
  }
  vars = paste("~", c(Y, X))
  vars = lapply(vars, FUN = function(v) all.vars(stats::as.formula(v)))
  vars = unique(unlist(vars))
  if (!is.null(data) && !all(vars %in% names(data)) && intersection) {
    warning("some 'formula' variables are not present in 'data' columns, 
             intersection of both is returned")
    vars = intersect(vars, names(data))
  }
  vars
}
