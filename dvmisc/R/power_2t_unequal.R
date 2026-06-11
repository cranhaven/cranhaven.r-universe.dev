#' Calculate Power for Two-Sample Unequal Variance T-Test
#'
#' Unequal variance version of \code{\link{power_2t_equal}}. Assumes an equal
#' sample size for both groups, which is actually not optimal.
#'
#'
#' @param n Numeric value specifying per-group sample size.
#' @param d Numeric value specifying true difference in group means. Should be
#' positive.
#' @param sigsq1,sigsq2 Numeric value specifying the variance of observations in
#' each group.
#' @param alpha Numeric value specifying type-1 error rate.
#'
#'
#' @return Numeric value.
#'
#'
#' @examples
#' # Power to detect difference of 0.2 with 100 subjects per group and sigsq's
#' # of 1 and 1.25
#' power_2t_unequal(n = 100, d = 0.2, sigsq1 = 1, sigsq2 = 1.25)
#'
#' @export
power_2t_unequal <- function(n = 100, d, sigsq1, sigsq2, alpha = 0.05) {
  
  # Make sure d is positive
  if (! (d > 0)) {
    if (d == 0) {
      stop("d cannot be 0")
    } else {
      d <- abs(d)
    }
  }

  # Degrees of freedom and non-centrality parameter
  df <- (n - 1) * (sigsq1 + sigsq2)^2 / (sigsq1^2 + sigsq2^2)
  ncp <- d / sqrt((sigsq1 + sigsq2) / n)

  # Power
  pt(
    q = qt(p = 1 - alpha / 2, df = df),
    df = df,
    ncp = ncp,
    lower.tail = FALSE
  )

}
