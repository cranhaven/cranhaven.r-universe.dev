#' Quantile function half Cauchy
#'
#' Computes the quantiles.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(p, location = 0, scale = 1) {
#'   qcauchy(p * (1 - pcauchy(0, location, scale)) + pcauchy(
#'     0,
#'     location, scale
#'   ), location, scale)
#' }
qhalfcauchy <-
  function(p, location = 0, scale = 1) {
    qcauchy(p * (1 - pcauchy(0, location, scale)) + pcauchy(
      0,
      location, scale
    ), location, scale)
  }
