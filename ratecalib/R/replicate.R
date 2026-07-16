# Replicate-weight variance estimation for calibrated weights.

#' Re-calibrate replicate weights
#'
#' Re-runs the calibration of a fitted `pass_rate_calibration` object for each
#' set of replicate weights (e.g. bootstrap, jackknife or BRR weights produced
#' elsewhere), holding the targets and solver settings fixed. The resulting
#' calibrated replicate weights feed [replicate_variance()] for design-consistent
#' variance estimation, mirroring the replicate-weights approach of the `survey`
#' package.
#'
#' @param fit A fitted object of class `pass_rate_calibration`.
#' @param repweights A numeric matrix or data frame of replicate weights with
#'   one row per observation (matching `fit`) and one column per replicate. All
#'   entries must be finite and positive.
#' @param scale,rscales Replication variance constants, as in
#'   `survey::svrepdesign()`. `Var = scale * sum_r rscales_r * (theta_r -
#'   theta_0)^2`. Set them to match your replication scheme (for example JK1:
#'   `scale = (R-1)/R`, `rscales = 1`; bootstrap: `scale = 1/R`). `rscales`
#'   defaults to a vector of ones.
#' @param progress Logical; show a text progress bar over the replicates.
#'
#' @return An object of class `replicate_calibration` with the full-sample
#'   calibrated `weights`, the matrix of calibrated `replicate_weights`, and the
#'   `scale`/`rscales` constants.
#' @seealso [replicate_variance()]
#' @examples
#' d <- example_rate_data(300)
#' fit <- calibrate_rates(d, "qualified", "initial_weight",
#'                        groups = list(sex = c(M = 0.72, F = 0.68)))
#' repw <- d$initial_weight * matrix(stats::runif(nrow(d) * 5, 0.8, 1.2), ncol = 5)
#' rc <- calibrate_replicate_weights(fit, repw)
#' replicate_variance(rc, d$qualified, statistic = "mean")
#' @export
calibrate_replicate_weights <- function(fit, repweights, scale = 1,
                                        rscales = NULL, progress = FALSE) {
  if (!inherits(fit, "pass_rate_calibration")) {
    stop("fit must be a pass_rate_calibration object.", call. = FALSE)
  }
  repweights <- as.matrix(repweights)
  n <- nrow(repweights)
  R <- ncol(repweights)
  if (n != nrow(fit$data)) {
    stop("repweights must have one row per observation in fit.", call. = FALSE)
  }
  if (R < 1L) stop("repweights must have at least one column.", call. = FALSE)
  if (any(!is.finite(repweights)) || any(repweights <= 0)) {
    stop("Replicate weights must all be finite and positive.", call. = FALSE)
  }
  if (is.null(rscales)) rscales <- rep(1, R)
  if (length(rscales) != R) {
    stop("rscales must have one value per replicate.", call. = FALSE)
  }

  s <- fit$settings
  tc <- fit$target_check
  targets <- tc[, c("variable", "level", "target_rate", "priority",
                    "statistic", "value_var", "value"), drop = FALSE]
  base_data <- fit$data
  base_data[[s$new_weight]] <- NULL

  out <- matrix(NA_real_, n, R)
  if (isTRUE(progress)) pb <- utils::txtProgressBar(min = 0, max = R, style = 3)
  for (r in seq_len(R)) {
    dr <- base_data
    dr[[s$weight]] <- repweights[, r]
    fr <- tryCatch(
      calibrate_pass_rates(dr, outcome = s$outcome, weight = s$weight,
                           group_vars = s$group_vars, targets = targets,
                           lower = s$lower, upper = s$upper, mode = s$mode,
                           distance = s$distance, lambda = s$lambda,
                           new_weight = s$new_weight),
      error = function(e) {
        stop("Calibration failed for replicate ", r, ": ", conditionMessage(e),
             " Consider mode='soft' for robust replicate calibration.",
             call. = FALSE)
      })
    out[, r] <- fr$data[[s$new_weight]]
    if (isTRUE(progress)) utils::setTxtProgressBar(pb, r)
  }
  if (isTRUE(progress)) close(pb)

  structure(list(
    weights = fit$data[[s$new_weight]],
    replicate_weights = out,
    scale = scale,
    rscales = rscales,
    settings = s
  ), class = "replicate_calibration")
}

#' Replicate-weight variance of a calibrated estimate
#'
#' Computes the point estimate, variance and standard error of a weighted total
#' or mean of a study variable, using calibrated replicate weights.
#'
#' @param object A `replicate_calibration` object from
#'   [calibrate_replicate_weights()].
#' @param x Numeric study variable, one value per observation.
#' @param statistic Either `"total"` (weighted sum) or `"mean"` (weighted mean).
#'
#' @return A list with `estimate`, `variance` and `se`.
#' @examples
#' d <- example_rate_data(300)
#' fit <- calibrate_rates(d, "qualified", "initial_weight",
#'                        groups = list(sex = c(M = 0.72, F = 0.68)))
#' repw <- d$initial_weight * matrix(stats::runif(nrow(d) * 5, 0.8, 1.2), ncol = 5)
#' rc <- calibrate_replicate_weights(fit, repw)
#' replicate_variance(rc, d$qualified, statistic = "mean")
#' @export
replicate_variance <- function(object, x, statistic = c("total", "mean")) {
  statistic <- match.arg(statistic)
  if (!inherits(object, "replicate_calibration")) {
    stop("object must be a replicate_calibration object.", call. = FALSE)
  }
  x <- as.numeric(x)
  if (length(x) != length(object$weights)) {
    stop("x must have one value per observation.", call. = FALSE)
  }
  est <- function(w) {
    if (statistic == "total") sum(w * x) else sum(w * x) / sum(w)
  }
  theta0 <- est(object$weights)
  theta_r <- apply(object$replicate_weights, 2L, est)
  variance <- object$scale * sum(object$rscales * (theta_r - theta0)^2)
  list(estimate = theta0, variance = variance, se = sqrt(variance))
}

#' @rdname calibrate_replicate_weights
#' @param x A `replicate_calibration` object (for the print method).
#' @param ... Ignored.
#' @export
print.replicate_calibration <- function(x, ...) {
  cat("ratecalib replicate calibration\n")
  cat("  Replicates:   ", ncol(x$replicate_weights), "\n", sep = "")
  cat("  Observations: ", nrow(x$replicate_weights), "\n", sep = "")
  cat("  scale:        ", format(x$scale), "\n", sep = "")
  cat("Use replicate_variance() to estimate variances.\n")
  invisible(x)
}
