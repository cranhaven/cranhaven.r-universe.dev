#' Conditional predictive ordinate function
#'
#' This function computes conditional predictive ordinates for each data point.
#'
#' For internal use.
#'
#' @keywords internal
#' @examples
#'
#' ## The function is currently defined as
#' function(obj) {
#'   fx <- obj$fx
#'   cpo <- 1 / apply(1 / fx, 1, mean)
#'   return(cpo)
#' }
cpo <-
  function(obj) {
    fx <- obj$fx
    cpo <- 1 / apply(1 / fx, 1, mean)
    return(cpo)
  }
