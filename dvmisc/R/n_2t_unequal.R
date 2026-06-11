#' Calculate Per-Group Sample Size for Two-Sample Unequal Variance T-Test
#'
#' Unequal variance version of \code{\link{n_2t_equal}}. Assumes an equal sample
#' size for both groups, which is actually not optimal.
#'
#'
#' @param d Numeric value specifying true difference in group means.
#' @param sigsq1,sigsq2 Numeric value specifying the variance of observations in each group.
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
#' # sigsq's of 1 and 1.25
#' n_2t_unequal(d = 0.2, sigsq1 = 1, sigsq2 = 1.25, beta = 0.1)
#'
#'
#' @export
n_2t_unequal <- function(d, sigsq1, sigsq2, alpha = 0.05, beta = 0.2) {
  
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
  n <- ceiling(zstar^2 / d^2 * (sigsq1 + sigsq2)) - 1

  # Increment until sufficient power is achieved
  power.value <- 0
  var.term <- (sigsq1 + sigsq2)^2 / (sigsq1^2 + sigsq2^2)
  while (power.value < (1 - beta)) {
    n <- n + 1
    df <- (n - 1) * var.term
    ncp <- d / sqrt((sigsq1 + sigsq2) / n)
    cutpoint <- qt(p = 1 - alpha / 2, df = df)
    power.value <- pt(q = cutpoint, df = df, ncp = ncp, lower.tail = FALSE)
  }
  return(n)

}
