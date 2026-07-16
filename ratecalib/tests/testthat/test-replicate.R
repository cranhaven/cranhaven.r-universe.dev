# Tests for replicate-weight variance estimation: re-calibrate each set of
# replicate weights to the same targets, then estimate the variance of an
# estimator via the general replicate formula
#   Var = scale * sum_r rscales_r * (theta_r - theta_0)^2.

skip_if_not_installed("osqp")

make_fit <- function(n = 600, seed = 3) {
  d <- example_rate_data(n = n, seed = seed)
  # An overall-only exact target (sex kept only as a preserved margin) is
  # jointly feasible and pins the overall rate exactly in every replicate.
  calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "sex",
                       targets = make_rate_targets(overall = 0.70),
                       mode = "exact")
}

rep_weights <- function(fit, R = 12, seed = 5) {
  set.seed(seed)
  w0 <- fit$data[[fit$settings$weight]]
  matrix(w0 * exp(stats::rnorm(length(w0) * R, 0, 0.2)), ncol = R)
}

test_that("calibrate_replicate_weights returns one calibrated column per replicate", {
  fit <- make_fit()
  rw <- rep_weights(fit, R = 8)
  rc <- calibrate_replicate_weights(fit, rw)
  expect_s3_class(rc, "replicate_calibration")
  expect_equal(ncol(rc$replicate_weights), 8)
  expect_equal(nrow(rc$replicate_weights), nrow(fit$data))
  expect_true(all(rc$replicate_weights > 0))
})

test_that("an exactly-targeted quantity has near-zero replicate variance", {
  fit <- make_fit()
  rc <- calibrate_replicate_weights(fit, rep_weights(fit))
  y <- fit$data$qualified
  # every replicate hits the overall target rate, so the weighted mean of y is
  # essentially constant across replicates
  v <- replicate_variance(rc, y, statistic = "mean")
  expect_equal(v$estimate, 0.70, tolerance = 1e-6)
  expect_lt(v$se, 1e-6)
})

test_that("a non-targeted variable has positive replicate variance", {
  fit <- make_fit()
  rc <- calibrate_replicate_weights(fit, rep_weights(fit))
  set.seed(1); z <- stats::rnorm(nrow(fit$data))
  v <- replicate_variance(rc, z, statistic = "total")
  expect_gt(v$variance, 0)
  expect_equal(v$se, sqrt(v$variance))
})

test_that("variance scales linearly with the scale constant", {
  fit <- make_fit()
  rc1 <- calibrate_replicate_weights(fit, rep_weights(fit), scale = 1)
  rc2 <- calibrate_replicate_weights(fit, rep_weights(fit), scale = 2)
  z <- fit$data$initial_weight
  expect_equal(replicate_variance(rc2, z, "total")$variance,
               2 * replicate_variance(rc1, z, "total")$variance,
               tolerance = 1e-8)
})

test_that("identical replicate weights give zero variance", {
  fit <- make_fit()
  w0 <- fit$data[[fit$settings$weight]]
  rw <- matrix(rep(w0, 5), ncol = 5)  # every replicate equals the original
  rc <- calibrate_replicate_weights(fit, rw)
  v <- replicate_variance(rc, fit$data$qualified, "total")
  expect_equal(v$variance, 0, tolerance = 1e-8)
})

test_that("replicate weights with the wrong number of rows error", {
  fit <- make_fit()
  bad <- matrix(1, nrow = 5, ncol = 3)
  expect_error(calibrate_replicate_weights(fit, bad), "row")
})

test_that("non-positive replicate weights error", {
  fit <- make_fit()
  rw <- rep_weights(fit, R = 3)
  rw[1, 1] <- 0
  expect_error(calibrate_replicate_weights(fit, rw), "positive")
})

test_that("progress = TRUE runs without error", {
  fit <- make_fit()
  expect_silent(
    suppressMessages(
      utils::capture.output(
        calibrate_replicate_weights(fit, rep_weights(fit, R = 4), progress = TRUE))))
})
