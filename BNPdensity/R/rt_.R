#' Random number generator non-standard Student-t
#'
#' Computes a random number.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(n, df, mean, sd) {
#'   mean + sd * rt(n, df, ncp = 0)
#' }
rt_ <-
  function(n, df, mean, sd) {
    mean + sd * rt(n, df, ncp = 0)
  }
