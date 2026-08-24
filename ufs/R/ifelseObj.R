#' Conditional returning of an object
#'
#' The ifelseObj function just evaluates a condition, returning
#'   one object if it's true, and another if it's false.
#'
#' @param condition Condition to evaluate.
#' @param ifTrue Object to return if the condition is true.
#' @param ifFalse Object to return if the condition is false.
#'
#' @return One of the two objects
#' @export
#'
#' @examples dat <- ifelseObj(sample(c(TRUE, FALSE), 1), mtcars, Orange);
#'

ifelseObj <- function(condition, ifTrue, ifFalse) {
  if (condition) {
    return(ifTrue);
  }
  else {
    return(ifFalse);
  }
}
