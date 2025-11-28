#' Calculates asymptotic confidence intervals.
#'
#' @param x Data to estimate alpha on.
#' @param est,sd The estimate and estimated standard deviation.
#' @param n Number of observations.
#' @param type Type of confidence interval. Either `adf`, `elliptical`, or
#'   `normal`.
#' @param transformer A transformer object.
#' @param parallel If `TRUE`, makes calculations under the assumption of a
#'   parallel model. Default to `FALSE`.
#' @param quants Quantiles for the confidence interval.
#' @param n_reps Number of bootstrap samples if `bootstrap = TRUE`. Ignored if
#'   `bootstrap = FALSE`.
#' @param standardized If `TRUE`, calculates the standardized alpha. Calculates
#'   coefficient alpha otherwise.
#' @keywords internal
#' @name ci
ci_asymptotic <- function(est, sd, n, transformer, quants) {
  est_t <- transformer$est(est)
  sd_t <- transformer$sd(est, sd)
  multiplier <- stats::qt(quants, n - 1) / sqrt(n - 1)
  sort(transformer$inv(est_t + multiplier * sd_t))
}

#' @keywords internal
#' @rdname ci
ci_boot <- function(x,
                    est,
                    sd,
                    type,
                    transformer,
                    parallel,
                    quants,
                    n_reps,
                    standardized = FALSE) {
  boots <- studentized_boots(n_reps, x, type, parallel, transformer)
  est_t <- transformer$est(est)
  sd_t <- transformer$sd(est, sd)
  multiplier <- stats::quantile(boots, quants, na.rm = TRUE)
  sort(transformer$inv(est_t + multiplier * sd_t))
}

#' Studentized bootstrap estimates using transformers.
#'
#' @param n_reps Number of bootstrap repetitions.
#' @param x Data to estimate alpha on.
#' @param type Type of confidence interval. Either `adf`, `elliptical`, or
#'   `normal`.
#' @param transformer A transformer object.
#' @param parallel If `TRUE`, makes calculations under the assumption of a
#'   parallel model.
#' @param transformer A `transformer` object.
#' @param standardized If `TRUE`, calculates the standardized alpha. Calculates
#'   coefficient alpha otherwise.
#' @return Studentized bootstrap estimates.
#' @keywords internal
studentized_boots <- function(n_reps,
                              x,
                              type,
                              parallel,
                              transformer,
                              standardized = FALSE) {
  fun <- if (standardized) alpha_std else alpha
  est <- fun(stats::cov(x))
  future.apply::future_replicate(n_reps,
    {
      indices_star <- sample(nrow(x), nrow(x), replace = TRUE)
      sigma_star <- stats::cov(x[indices_star, ])
      est_star <- fun(sigma_star)
      sd_star <- sqrt(avar(x[indices_star, ], sigma_star, type, parallel))
      (transformer$est(est_star) - transformer$est(est)) /
        transformer$sd(est_star, sd_star)
    },
    future.seed = TRUE
  )
}

#' Calculate limits of a confidence interval.
#'
#' @param alternative Alternative choosen.
#' @param conf_level Confidence level.
#' @keywords internal
limits <- function(alternative, conf_level) {
  half <- (1 - conf_level) / 2
  if (alternative == "two.sided") {
    return(c(half, 1 - half))
  }
  if (alternative == "greater") {
    return(c(2 * half, 1))
  }
  if (alternative == "less") {
    return(c(0, conf_level))
  }
}
