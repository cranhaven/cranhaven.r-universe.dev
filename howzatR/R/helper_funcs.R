
# Modulo & Integer Division Calculation -----------------------------------

#' Calculates Modulo
#'
#' Calculates remainder of a division. See \url{https://en.wikipedia.org/wiki/Modulo_operation} for more details
#'
#' @param value number to divide through
#' @param divisor number to use or the division
#'
#' @return remainder of division
#' @noRd
#'
#' @examples
#' calc_mod(value = 5.5, divisor = 1)
calc_mod <- function(value, divisor) {
  value %% divisor
}

#' Calculates Integer Division
#'
#' Calculates division and discards the remainder. See \url{https://mathworld.wolfram.com/IntegerDivision.html} for more details
#'
#' @param value number to divide through
#' @param divisor number to use or the division
#'
#' @return division with remainder discarded; leaving just the integer
#' @noRd
#'
#' @examples
#' calc_int_div(value = 5.5, divisor = 1)
calc_int_div <- function(value, divisor) {
  value %/% divisor
}
