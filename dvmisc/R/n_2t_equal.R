#' Calculate Per-Group Sample Size for Two-Sample Equal Variance T-Test
#'
#' Same idea as \code{\link[stats]{power.t.test}}. Less flexible, but faster.
#'
#'
#' @param d Numeric value specifying true difference in group means.
#' @param sigsq Numeric value specifying the variance of observations.
#' @param alpha Numeric value specifying type-1 error rate.
#' @param beta Numeric value specifying type-2 error rate.
#'
#'
#' @return Numeric value indicating per-group sample size, rounded up to the
#' nearest whole number.
#'
#'
#' @examples
#' # Per-group sample size for 90% power to detect difference of 0.2 with
#' # sigsq = 1
#' n_2t_equal(d = 0.2, sigsq = 1, beta = 0.1)
#'
#'
#' @export
n_2t_equal <- function(d, sigsq, alpha = 0.05, beta = 0.2) {
  
  # Make sure d is positive
  if (! (d > 0)) {
    if (d == 0) {
      stop("d cannot be 0")
    } else {
      d <- abs(d)
    }
  }

  # Get lower bound for n based on Z formula
  zstar <- qnorm(p = 1 - beta) + qnorm(p = 1 - alpha / 2)
  n <- ceiling(2 * zstar^2 / d^2 * sigsq) - 1

  # Increment until sufficient power is achieved
  power.value <- 0
  while (power.value < (1 - beta)) {
    n <- n + 1
    df <- 2 * n - 2
    ncp <- d / sqrt(2 * sigsq / n)
    cutpoint <- qt(p = 1 - alpha / 2, df = df)
    power.value <- pt(q = cutpoint, df = df, ncp = ncp, lower.tail = FALSE)
  }
  return(n)

}
