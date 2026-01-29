#' Random number generator half Student-t
#'
#' Generates a random number.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(n, df = 1, mean = 0, sd = 1) {
#'   abs(rt_(n, df, mean, sd))
#' }
rhalft <-
  function(n, df = 1, mean = 0, sd = 1) {
    abs(rt_(n, df, mean, sd))
  }
