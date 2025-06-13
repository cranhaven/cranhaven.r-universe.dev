#' Test for Underestimated Processing Error Variance in Pooling Studies
#'
#' In studies where a biomarker is measured in combined samples from multiple
#' subjects rather than for each individual, design parameters (e.g. optimal
#' pool size, sample size for 80\% power) are very sensitive to the magnitude of
#' processing errors. This function provides a test that can be used midway
#' through data collection to test whether the processing error variance is
#' larger than initially assumed, in which case the pool size may need to be
#' adjusted.
#'
#' The method is fully described in a manuscript currently under review.
#' Briefly, the test of interest is \code{H0: sigsq_p <= c}, where
#' \code{sigsq_p} is the processing error variance and \code{c} is the value
#' assumed during study design. Under additive errors, a point estimate for
#' \code{sigsq_p} is given by:
#'
#' \code{sigsq_p.hat = s2 - sigsq / g - sigsq_m}
#'
#' where \code{s2} is the sample variance of poolwise measurements, \code{g} is
#' the pool size, and \code{sigsq_m} is the measurement error variance which may
#' be 0 if the assay is known to be precise.
#'
#' Under multiplicative errors, the estimator is:
#'
#' \code{sigsq_p.hat = [(s2 - sigsq / g) / (mu^2 + sigsq / g) - sigsq_m] /
#' (1 + sigsq_m)}.
#'
#' In either case, bootstrapping can be used to obtain a lower bound for a
#' one-sided confidence interval. If the lower bound is greater than \code{c},
#' \code{H0} is rejected.
#'
#'
#' @inheritParams poolcost_t
#'
#' @param xtilde Numeric vector of pooled measurements.
#' @param g Numeric value specifying the pool size.
#' @param mu Numeric value specifying the mean of observations. Only used if
#' \code{multiplicative = TRUE}.
#' @param alpha Numeric value specifying significance level for bootstrap
#' confidence interval.
#' @param boots Numeric value specifying the number of bootstrap samples to
#' take.
#' @param seed Numeric value specifying the random number seed, in case it is
#' important to be able to reproduce the lower bound.
#'
#'
#' @return List containing point estimate and lower bound of confidence
#' interval.
#'
#'
#' @examples
#' # Generate data for hypothetical study designed assuming sigsq_p = 0.1, but
#' # truly sigsq_p = 0.25. Have data collected for 40 pools of size 5, and wish
#' # to test H0: sigsq_p <= 0.1. In this instance, a false negative occurs.
#' set.seed(123)
#' xtilde <- replicate(n = 40, expr = mean(rnorm(5)) + rnorm(n = 1, sd = sqrt(0.25)))
#' (fit <- test_pe(xtilde = xtilde, g = 5, sigsq = 1, sigsq_m = 0))
#'
#'
#' @export
test_pe <- function(xtilde,
                    g,
                    sigsq,
                    sigsq_m = 0,
                    multiplicative = FALSE,
                    mu = NULL,
                    alpha = 0.05,
                    boots = 1000,
                    seed = NULL) {

  # Calculate s2 and n
  s2 <- var(xtilde)
  n <- length(xtilde)

  if (multiplicative) {

    # Point estimate for sigsq_p
    sigsq_p.hat <- ((s2 - sigsq / g) / (mu^2 + sigsq / g) - sigsq_m) /
      (1 + sigsq_m)

    # Bootstrap lower bound
    sigsq_p.hats <- replicate(
      n = boots,
      expr = ((var(sample(xtilde, size = n, replace = TRUE)) - sigsq / g) /
           (mu^2 + sigsq / g) - sigsq_m) /
        (1 + sigsq_m)
    )
    lower.bound <- quantile(sigsq_p.hats, probs = alpha)

  } else {

    # Point estimate for sigsq_p
    sigsq_p.hat <- s2 - sigsq / g - sigsq_m

    # Bootstrap lower bound
    sigsq_p.hats <- replicate(
      n = boots,
      expr = var(sample(xtilde, size = n, replace = TRUE)) - sigsq / g - sigsq_m
    )
    lower.bound <- quantile(sigsq_p.hats, probs = alpha)

  }

  # Return list
  ret.list <- list(sigsq_p.hat = sigsq_p.hat,
                   lower.bound = as.numeric(lower.bound))
  return(ret.list)

}
