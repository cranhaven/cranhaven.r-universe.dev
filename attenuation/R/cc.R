#' Confidence curves for attenuated correlation coefficients.
#'
#' @param r Numeric vector of three elements in [-1,1]. \code{r[1]} is the
#'     correlation between the noisy measures X' and Y', \code{r[2]} is the
#'     correlation between the noisy X' and the true X, while \code{r[3]} is
#'     the correlation between the noisy Y' and the true Y.
#' @param N Numeric vector of three positive integers. \code{N[i]} is the
#'     sample size for \code{r[i]}.
#' @param lower Lower bound for the curve. Defaults to -1.
#' @param upper Upper bound for the curve. Defaults to 1.
#' @param by Increment of the sequence from \code{lower} to \code{upper}.
#' @param method The type of confidence curve. Can be \code{"corr"},
#'     \code{"cronbach"}, \code{"HS"} or \code{"free"}. See the details of
#'     \code{\link{p_value}}.
#' @param k Numeric vector of two positive integers. \code{k[i]} is the number
#'     of testlets for the for \code{r[i+1]}. Only needed for method
#'     \code{"cronbach"}.
#' @return An object of class \code{ccaf}.
#' @examples
#'     r = c(0.20, sqrt(0.45), sqrt(0.55))
#'     N = c(100, 100, 100)
#'     plot(cc(r, N))
#' @export

cc = function(r, N, lower = -1, upper = 1, by = 0.001,
              method = "corr", k = NULL) {

  rho = seq(lower, upper, by = by)
  x = 1 - p_value(rho, r, N, method = method, k = k)
  attr(x, "type") = "Confidence curve"
  x

}