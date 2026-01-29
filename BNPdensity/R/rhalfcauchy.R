#' Random number generator half Cauchy
#'
#' Computes a random number.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(n, location = 0, scale = 1) {
#'   abs(rcauchy(n, location, scale))
#' }
rhalfcauchy <-
  function(n, location = 0, scale = 1) {
    abs(rcauchy(n, location, scale))
  }
