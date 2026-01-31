#' @title Neat rounding
#'
#' @description Rounds a given number to given number of digits after the
#'   decimal point, returning it as string, with trailing zeros when applicable.
#' @param num Number to be rounded.
#' @param round_to Number of fractional digits (i.e., digits after the decimal
#'   point), to round to.
#' @param leading_zero Logical, \code{TRUE} by default. If \code{FALSE}, omits
#'   leading zero (e.g. returns ".17" instead of "0.17").
#' @param signi Logical, \code{FALSE} by default. If \code{TRUE}, rounds to a
#'   fractional digit that allows at least the first \code{N} non-zero digits
#'   displayed in all numbers, where \code{N} is specified by the
#'   \code{round_to} parameter. (See \code{\link[base:formatC]{base:formatC}})
#' @return Number as string: \code{num} rounded to \code{round_to} digits, with
#'   trailing zeros when applicable.
#' @examples
#' ro( 1.2345 ) # returns "1.23"
#'
#' ro( 0.12345, 1 ) # returns "0.1"
#'
#' ro( 12.3, 4 ) # returns "12.3000"
#'
#' # examples with vectors
#' to_round = c(1000, 100, 0.1, 0.01, 0.001, 0.0001)
#' ro(to_round)
#' ro(to_round, 3)
#' ro(to_round, 3, leading_zero = FALSE)
#' ro(to_round, 3, signi = TRUE)
#' to_round2 = c(1230.000, 100, 0.012, 0.01, 0.123, 0.012340)
#' ro(to_round2, 3)
#' ro(to_round2, 3, signi = TRUE)
#' ro(to_round2, 2, signi = TRUE)
#' ro(to_round2, 1, signi = TRUE)
#' ro(to_round2, 9, signi = TRUE)
#'
#'
#' @export
ro = function(num,
              round_to = 2,
              leading_zero = TRUE,
              signi = FALSE) {
    validate_args(match.call(),
                  list(val_arg(leading_zero, c('bool'), 1),
                       val_arg(signi, c('bool'), 1)))
    if (is.numeric(num)) {
        value = num
    } else {
        value = as.numeric(as.character(num))
    }
    if (signi == TRUE) {
        value = base::formatC(value, format = "fg", digits = round_to)
    } else {
        value = format(round(value, round_to),
                       nsmall = round_to,
                       scientific = FALSE)
    }
    formtd = gsub(" ", "", value)
    if (leading_zero == FALSE) {
        formtd = sub("0.", ".", formtd, fixed = TRUE)
    }
    return(formtd)
}
