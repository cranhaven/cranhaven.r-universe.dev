#' Quantile function non-standard Student-t
#'
#' Computes the quantiles.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(p, df, mean, sd) {
#'   sd * qt(p, df, ncp = 0) + mean
#' }
qt_ <-
  function(p, df, mean, sd) {
    sd * qt(p, df, ncp = 0) + mean
  }
