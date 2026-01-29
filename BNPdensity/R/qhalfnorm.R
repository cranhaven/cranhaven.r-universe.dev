#' Quantile function half Normal
#'
#' Computes the quantiles.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(p, mean = 0, sd = 1) {
#'   qnorm(
#'     p * (1 - pnorm(0, mean, sd)) + pnorm(0, mean, sd),
#'     mean, sd
#'   )
#' }
qhalfnorm <-
  function(p, mean = 0, sd = 1) {
    qnorm(
      p * (1 - pnorm(0, mean, sd)) + pnorm(0, mean, sd),
      mean, sd
    )
  }
