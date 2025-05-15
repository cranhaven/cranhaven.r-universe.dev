#' @title Calculate Gamma parameters
#' @description Calculate Gamma rate and shape parameters from 
#'   mode and variance.
#'
#' @param m mode
#' @param v variance
#'
#' @export

gammaParams <- function(m, v) {
  t(mapply(
    function(m, v) {
      rate <- (m + sqrt(m ^ 2 + 4 * v) ) / (2 * v)
      shape <- 1 + m * rate
      c(gamma.rate = rate, gamma.shape = shape)
    },
    m = m,
    v = v
  ))
}
