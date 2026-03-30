#' @title Power Calculation for the Exact Binomial Test
#'
#' @description
#' Calculates the power of a one-sample exact binomial test via simulation.
#' This is used to compare a treatment response rate against a fixed
#' historical benchmark.
#'
#' @param n Sample size of the experimental group.
#' @param p Assumed true proportion (response rate) of the experimental treatment.
#' @param p0 Historical proportion (null hypothesis) used as a benchmark.
#' @param alpha Type I error rate (significance level).
#' @param alternative Character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less".
#' @param nsim Number of simulated trials; defaults to 100,000 for high precision.
#' @param seed Seed for reproducibility.
#'
#' @return A numeric value representing the statistical power.
#'
#' @export
#'
#' @examples
#' \donttest{
#' power.binom.test(n=110, p=0.4, p0=0.128)
#' }
#'
power.binom.test <- function(n = 20, p = 0.5, p0 = 0.3, alpha = 0.05,
                             alternative = c("two.sided", "greater", "less"),
                             nsim = 100000, seed = 2025) {

  alternative <- match.arg(alternative)
  set.seed(seed)

  # Simulate number of successes
  x <- rbinom(n = nsim, size = n, prob = p)

  # Vectorized calculation of p-values using the Binomial Distribution
  if (alternative == "less") {
    p.values <- pbinom(x, n, p0)
  } else if (alternative == "greater") {
    p.values <- pbinom(x - 1, n, p0, lower.tail = FALSE)
  } else {
    p.values <- vapply(x, function(successes) {
      binom.test(x = successes, n = n, p = p0, alternative = "two.sided")$p.value
    }, numeric(1))
  }

  power <- sum(p.values <= alpha) / nsim
  return(power)
}
