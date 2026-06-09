#' Calculate a confidence interval for an attenuated correlation coefficient.
#'
#' @param r Numeric vector of three elements in [-1,1]. \code{r[1]} is the
#'     correlation between the noisy measures X' and Y', \code{r[2]} is the
#'     correlation between the noisy X' and the true X, while \code{r[3]} is
#'     the correlation between the noisy Y' and the true Y.
#' @param N Numeric vector of three positive integers. \code{N[i]} is the
#'     sample size for \code{r[i]}.
#' @param level Numeric in [0, 1]. Confidence level of the interval. Defaults to
#'     0.95.
#' @param method The type of confidence curve. Can be \code{"corr"},
#'     \code{"cronbach"}, \code{"HS"} or \code{"free"}. See the details of
#'     \code{\link{p_value}}.
#' @param k Numeric vector of two positive integers. \code{k[i]} is the number
#'     of testlets for the for \code{r[i+1]}. Only needed for method
#'     \code{"cronbach"}.
#' @return Numeric in [0, 1]. The p-value under null-hypothesis rho.
#' @examples
#'     r = c(0.20, sqrt(0.45), sqrt(0.55))
#'     N = c(100, 100, 100)
#'     ci(r, N) # Calculates 95% confidence set for rho.
#' @export

ci = function(r, N, level = 0.95, method = "corr", k = NULL) {

  if (method == "HS") {
    z = -stats::qnorm((1 - level) / 2)
    sigma = (1 - r[1] ^ 2) / sqrt(N[1] - 1)
    lower = (r[1] - z * sigma) / (r[2] * r[3])
    upper = (r[1] + z * sigma) / (r[2] * r[3])
    if (lower >= 1 | upper <= -1) NULL else c(max(lower, -1), min(upper, 1))
  }

  alpha = 1 - level

  fun = function(rho, r, N) p_value(rho, r, N, method = method, k = k)

  ## If the maximum is less than -(1 - alpha), the CI is  c(-1,1)
  maximum = stats::optimize(f = function(rho) as.numeric(1 - fun(rho, r, N)),
                     interval = c(-1, 1),
                     maximum = TRUE)

  if (maximum$objective < (1 - alpha)) return(c(-1, 1))

  ## If the minimum is larger than (1 - alpha), the CI is empty.
  minimum = stats::optimize(f = function(rho) as.numeric(1 - fun(rho, r, N)),
                     interval = c(-1, 1),
                     maximum = FALSE)

  if (minimum$objective > (1 - alpha)) return(NULL)

  f = function(rho) (fun(rho, r, N) - alpha)^2

  # If the the right edge is greater than alpha, the CI is connected.
  if ((1 - fun(1, r, N)) > (1 - alpha)) {
    if ((1 - fun(-1, r, N)) > (1 - alpha)) {
      s = r[1] / (r[2] * r[3])
      lower = stats::optimize(f = f,
                       interval = c(-1, s),
                       maximum = FALSE)$minimum

      upper = stats::optimize(f = f,
                       interval = c(s, 1),
                       maximum = FALSE)$minimum
      return(c(lower, upper))
    }

    upper = stats::optimize(f = f,
                     interval = c(-1, maximum$maximum),
                     maximum = FALSE)$minimum
    return(c(-1, upper))


  }

  # If the left edge is greater than alpha, the CI is connected. Since the
  # case of a bounded CI is covered above, it is unbounded to the right.

  if ((1 - fun(-1, r, N)) > (1 - alpha)) {
    lower = stats::optimize(f = f,
                     interval = c(maximum$maximum, 1),
                     maximum = FALSE)$minimum
    return(c(lower, 1))
  }

  # If no edges are are greater than alpha, the CI has two components.
  lower = stats::optimize(f = f,
                   interval = c(maximum$maximum, 1),
                   maximum = FALSE)$minimum

  upper = stats::optimize(f = f,
                   interval = c(-1, maximum$maximum),
                   maximum = FALSE)$minimum

  list(lower = c(-1, upper),
       upper = c(lower, 1))

}
