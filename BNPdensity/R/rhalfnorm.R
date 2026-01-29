#' Random number generator half Normal
#'
#' Computes a random number.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(n, mean = 0, sd = 1) {
#'   abs(rnorm(n, mean, sd))
#' }
rhalfnorm <-
  function(n, mean = 0, sd = 1) {
    abs(rnorm(n, mean, sd))
  }
