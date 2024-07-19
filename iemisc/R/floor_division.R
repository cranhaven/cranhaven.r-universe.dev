#' Floor Division (Python compatible)
#'
#' Performs floor (or integer) division on 2 numeric vectors.
#'
#'
#' @param x numeric vector
#' @param y numeric vector
#'
#'
#' @return The integral part of the quotient is returned.
#'
#'
#'
#'
#'
#'
#'
#' @references
#'
#' Jakub Przywóski: "Python Reference (The Right Way)". // floor division, 2015, Revision 9a3b94e7, \url{https://python-reference.readthedocs.io/en/latest/docs/operators/floor_division.html}.
#'
#'
#'
#'
#'
#'
#' @author Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#'
#'
#'
#' @examples
#' 
#' # Example 1 -- From the Python reference
#' 
#' library(iemisc)
#' 
#' 5.0 / 2
#' # 2.5
#' 
#' 5.0 %//% 2
#' # 2.0
#'
#'
#'
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom checkmate qtest
#'
#' @rdname floor_division
#' @export
`%//%` <- function(x, y) {

# Check
assert_that(!any(qtest(x, "N+(,)") == FALSE), msg = "x is NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

assert_that(!any(qtest(y, "N+(0,)") == FALSE), msg = "y is 0, NA, NaN, Inf, -Inf, empty, or a string. Please try again.")
# only process with finite values and provide an error message if the check fails

floor(x / y)

}
