#' Calculate Power for Two-Sample Equal Variance T-Test
#'
#' Same idea as \code{\link[stats]{power.t.test}}. Less flexible, but faster.
#'
#'
#' @param n Numeric value specifying per-group sample size.
#' @param d Numeric value specifying true difference in group means. Should be
#' positive.
#' @param sigsq Numeric value specifying the variance of observations.
#' @param alpha Numeric value specifying type-1 error rate.
#'
#'
#' @return Numeric value.
#'
#'
#' @examples
#' # Power to detect difference of 0.2 with 100 subjects per group and sigsq = 1
#' power_2t_equal(n = 100, d = 0.2, sigsq = 1)
#'
#' @export
power_2t_equal <- function(n = 100, d, sigsq, alpha = 0.05) {
  
  # Make sure d is positive
  if (! (d > 0)) {
    if (d == 0) {
      stop("d cannot be 0")
    } else {
      d <- abs(d)
    }
  }

  # Degrees of freedom and non-centrality parameter
  df <- 2 * n - 2
  ncp <- d / sqrt(2 * sigsq / n)

  # Power
  pt(
    q = qt(p = 1 - alpha / 2, df = df),
    df = df,
    ncp = ncp,
    lower.tail = FALSE
  )

}
