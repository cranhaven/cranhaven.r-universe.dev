#' Clopper-Pearson Exact Confidence Interval for a Binomial Proportion
#'
#' Calculates the two-sided Clopper-Pearson exact confidence interval for a
#' binomial proportion.
#'
#' @param r A scalar integer. The number of successes or responses.
#' @param n A scalar integer. The total number of trials or subjects.
#' @param conflev A scalar numeric. The desired confidence level (e.g., 0.95
#'   for a 95% CI).
#'
#' @return A numeric vector of length two containing the lower and upper
#'   confidence limits.
#'
#' @examples
#' exactci(r=4, n=20, conflev=0.95)
#'
#' @export
#'
exactci <- function(r, n, conflev) {
  alpha <- (1 - conflev)
  if (r == 0) {
    ll <- 0
    ul <- 1 - (alpha / 2)^(1 / n)
  }
  else if (r == n) {
    ll <- (alpha / 2)^(1 / n)
    ul <- 1
  }
  else {
    ll <- 1 / (1 + (n - r + 1) / (r * qf(alpha / 2, 2 * r, 2 * (n - r + 1))))
    ul <- 1 / (1 + (n - r) / ((r + 1) * qf(1 - alpha / 2, 2 * (r + 1), 2 * (n - r))))
  }
  c(ll, ul)
}

