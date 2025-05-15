#' @title Calculate Beta parameters
#' @description Calculate Beta shape parameters and variance from 
#'   mode and concentration.
#'
#' @param w mode
#' @param k concentration
#'
#' @export

betaParams <- function(w, k) {
  t(mapply(
    function(w, k) {
      a <- w * (k - 2) + 1
      B <- (1 - w) * (k - 2) + 1
      v <- (a * B) / ((a + B) ^ 2 * (a + B + 1))
      c(beta.a = a, beta.B = B, beta.v = v)
    },
    w = w,
    k = k
  ))
}
