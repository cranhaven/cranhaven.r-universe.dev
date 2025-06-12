#' @name print.OEFPIL
#' @title Print function for an object of class 'OEFPIL'
#' @description Function prints the information about an object of class \code{"OEFPIL"}.
#'
#' @param x an object of class \code{"OEFPIL"} (a result of a call to \code{\link{OEFPIL}}).
#' @param ... other arguments.
#'
#' @return Function prints short summary of \code{"OEFPIL"} object into the console. In case of assigning value into the variable, it returns object of class \code{"OEFPIL"}, which is a list with components defined in \link{OEFPIL}.
#' @seealso \code{\link{OEFPIL}}
#'
#' @examples
#' \dontshow{
#' utils::example("coef.OEFPIL",echo=FALSE)}
#' ##-- Continuing the coef.OEFPIL(.) example:
#'
#' ##Use of print function
#' print(st1)
#'
#' @export


print.OEFPIL <- function(x,...) {
  ## A print method for "OEFPIL".

  l <- (length(x) - 8) / 3
  ## number of parameters

  cat("OEFPIL\n\n")
  cat("Formula:\n")
  cat(x$contents$input.form.string)
  cat("\n\n")
  cat("Estimated parameters:\n")
  cat(unlist(x[1:l]))

  invisible(x)
}
