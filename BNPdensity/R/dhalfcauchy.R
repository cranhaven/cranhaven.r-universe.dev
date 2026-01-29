#' Density half Cauchy
#'
#' Computes the density.
#'
#' For internal use
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(x, location = 0, scale = 1) {
#'   ifelse(x < 0, 0, 1) * dcauchy(x, location, scale) / (1 - pcauchy(
#'     0,
#'     location, scale
#'   ))
#' }
dhalfcauchy <-
  function(x, location = 0, scale = 1) {
    ifelse(x < 0, 0, 1) * dcauchy(x, location, scale) / (1 - pcauchy(
      0,
      location, scale
    ))
  }
