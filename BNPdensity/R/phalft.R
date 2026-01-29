#' Distribution function half Student-t
#'
#' Computes the cumulative distribution function.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(q, df = 1, mean = 0, sd = 1) {
#'   ifelse(x < 0, 0, 1) * (pt_(q, df, mean, sd) - pt_(
#'     0, df,
#'     mean, sd
#'   )) / (1 - pt_(0, df, mean, sd))
#' }
phalft <-
  function(q, df = 1, mean = 0, sd = 1) {
    ifelse(q < 0, 0, 1) * (pt_(q, df, mean, sd) - pt_(
      0, df,
      mean, sd
    )) / (1 - pt_(0, df, mean, sd))
  }
