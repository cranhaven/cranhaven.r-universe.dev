#' Inverse the function f
#'
#'This function implements the numerical inverse of the function f.
#'
#' @param f the function to be inverted
#' @param lower a lower bound for the inverse
#' @param upper an lower bound for the inverse
#'
#' @return
#' @export
#'
#' @examples
inverse = function (f, lower = -3, upper = 3) {

  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
}
