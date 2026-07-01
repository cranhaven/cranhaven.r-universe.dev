#' is.wholenumber
#'
#' This function checks if the input is an integer
#'
#' @param x Numeric value
#' @param tol Tolerance
is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
#' @noRd
#' @keywords internal
