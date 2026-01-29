#' Density half Student-t
#'
#' Computes the density.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(x, df = 1, mean = 0, sd = 1) {
#'   ifelse(x < 0, 0, 1) * dt_(x, df, mean, sd) / (1 - pt_(
#'     0, df,
#'     mean, sd
#'   ))
#' }
dhalft <-
  function(x, df = 1, mean = 0, sd = 1) {
    ifelse(x < 0, 0, 1) * dt_(x, df, mean, sd) / (1 - pt_(
      0, df,
      mean, sd
    ))
  }
