#' To identify the minimum value out of two given sets of values
#'
#' This function is called in data.optim() or distr.optim()
#' which basically compares and returns the smaller value out
#' of two given sets of values.
#'
#' @param val1 A numeric: the first value
#' @param val2 A numeric: the second value
#'
#' @return \code{} returns the minimum value
#'
#' @author Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
#' MGM Khan <khan_mg@usp.ac.fj>
#'
minim.val <- function(val1, val2)
{
  if(val1 <= val2) val1
  else val2
}
###################################################
