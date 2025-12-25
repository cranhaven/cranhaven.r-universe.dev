#' This function the smoothed curve
#'
#' @param coef Fourier coefficients.
#' @param x a time series to evaluate the smoothed curve.
#'
#' @return a time series with the smoothed curve.
#' @import magrittr
#' @examples
#' data(canada)
#'
#' coefs <- coef_fourier(canada$ThePas_ts)
#' y_hat <- fourier_b(coefs)
#' @export
fourier_b <- function(coef, x) {
  if (missing(x)) x <- seq(from = -pi, to = pi, by = 0.01)

  #degree of Fourier Series
  ordem <- (length(coef) - 1) / 2

  matrix(x, ncol = 1) %>%
    apply(1, function(u) {
      valor <- coef[1] / 2
      for (k in 1:ordem) valor <- valor +
          coef[k + 1] * cos(k * u) + coef[k + ordem + 1] * sin(k * u)
      return(valor)
    })
}
