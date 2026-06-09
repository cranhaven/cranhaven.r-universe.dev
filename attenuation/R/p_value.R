#' Calculate the p-value for an attenuated correlation coefficient.
#'
#' This function calculates four types of p-values for correlations coefficients
#'     corrected for attenuation, chosen in "method". The different p-values
#'     are described in Moss (2019). \code{"corr"} is the correlation based
#'     p-value, \code{"cronbach"} is the Cronbach alpha based p-value,
#'     \code{"HS"} is the Hunter-Schmidt p-value, while \code{"free"} is the
#'     correlation based p-value without positive constraints.
#'
#' @param rho Numeric vector in [-1,1]. The correlation under the null
#'     hypothesis.
#' @param r Numeric vector of three elements in [-1,1]. \code{r[1]} is the
#'     correlation between the noisy measures X' and Y', \code{r[2]} is the
#'     correlation between the noisy X' and the true X, while \code{r[3]} is
#'     the correlation between the noisy Y' and the true Y. They are the
#'     square root of the reliabilities. Must be positive
#'     method to \code{"corr"} and \code{"cronbach"}.
#' @param N Numeric vector of three positive integers. \code{N[i]} is the
#'     sample size for \code{r[i]}.
#' @param method The type of p-value. Can be \code{"corr"}, \code{"cronbach"},
#'     \code{"HS"} or \code{"free"}. See the details.
#' @param k Numeric vector of two positive integers. \code{k[i]} is the number
#'     of testlets for the for \code{r[i+1]}. Only needed for method
#'     \code{"cronbach"}.
#' @return Numeric in [0, 1]. The p-value under the null-hypothesis that the
#'     true correlation is rho.
#' @examples
#'     r = c(0.20, sqrt(0.45), sqrt(0.55))
#'     N = c(100, 100, 100)
#'     p_value(rho = 0, r, N) # Tests rho = 0.
#' @export

p_value = function(rho, r, N, method = "corr", k = NULL) {

  if (method == "corr") {
    fun = function(rho) {

      D_inv = N - 3
      s = atanh(r)

      fn = function(theta) {
        eta = atanh(c(rho * theta[1] * theta[2], theta[1], theta[2]))
        c(D_inv %*% (eta - s) ^ 2)
      }

      eps = 0.01
      ui = diag(2)
      ci = rep(0, 2)
      start = pmax(abs(r[2:3]), eps) * (1 - eps)
      optimized = suppressWarnings(stats::constrOptim(theta = start,
                                                      f = fn,
                                                      grad = NULL,
                                                      ui = ui,
                                                      ci = ci))

      1 - stats::pchisq(optimized$value, df = 3)

    }
  } else if (method == "free") {
    fun = function(rho) {

      D_inv = N - 3
      s = atanh(r)

      fn = function(theta) {
        eta = atanh(c(rho * theta[1] * theta[2], theta[1], theta[2]))
        c(D_inv %*% (eta - s)^2)
      }

      eps = 0.01

      optimized = suppressWarnings(stats::optim(par = r[2:3] * (1 - eps),
                                                  fn = fn))

      1 - stats::pchisq(optimized$value, df = 3)

    }
  } else if (method == "cronbach") {
    fun = function(rho) {

      D_inv = c(N[1] - 3, 2 * N[2:3] * (k - 1) / k)
      s = c(atanh(r[1]),
            0.5 * log(1 - r[2]),
            0.5 * log(1 - r[3]))

      fn = function(theta) {
        eta = c(atanh(rho * sqrt(theta[1]) * sqrt(theta[2])),
                0.5 * log(1 - theta[1]),
                0.5 * log(1 - theta[2]))
        c(D_inv %*% (eta - s) ^ 2)
      }

      eps = 0.001
      optimized = suppressWarnings(stats::optim(par = abs(r[2:3]) * (1 - eps),
                                         fn = fn))

      1 - stats::pchisq(optimized$value, df = 3)

    }
  } else if (method == "HS") {

    fun = function(rho) {
      sigma = (1 - r[1] ^ 2) / sqrt(N[1] - 1)
      2 * stats::pnorm(-abs(r[1] - rho * (r[2] * r[3])), sd = sigma)
    }

  }

  values = sapply(rho, fun)

  class(values) = c("ccaf")
  attr(values, "rho") = rho
  attr(values, "N") = N
  attr(values, "r") = r
  attr(values, "type") = "p-value"
  attr(values, "method") = method
  values

}
