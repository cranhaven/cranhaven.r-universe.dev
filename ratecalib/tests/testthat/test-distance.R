# Tests for the distance-function family. chi2 (linear/quadratic, OSQP) is the
# default and current behaviour; raking (entropy) is an opt-in exact-mode
# calibration solved by the dual Newton method.

skip_if_not_installed("osqp")

feasible_fit <- function(distance = "chi2") {
  d <- example_rate_data(n = 1200, seed = 7)
  # residence is a margin-preserved grouping variable that is not targeted, so
  # the exact system is underdetermined and the distance function genuinely
  # affects how the spare degrees of freedom are distributed.
  calibrate_pass_rates(
    d, outcome = "qualified", weight = "initial_weight",
    group_vars = c("sex", "residence"),
    targets = make_rate_targets(groups = list(sex = c(M = 0.70, F = 0.65))),
    mode = "exact", distance = distance
  )
}

test_that("chi2 is the default distance and reproduces the existing solver", {
  d <- example_rate_data(n = 1200, seed = 7)
  args <- list(d, outcome = "qualified", weight = "initial_weight",
               group_vars = "sex",
               targets = make_rate_targets(groups = list(sex = c(M = 0.70, F = 0.65))),
               mode = "exact")
  default_fit <- do.call(calibrate_pass_rates, args)
  chi2_fit <- do.call(calibrate_pass_rates, c(args, list(distance = "chi2")))
  expect_equal(default_fit$data$weight_calibrated, chi2_fit$data$weight_calibrated)
})

test_that("raking exact hits the targets exactly", {
  fit <- feasible_fit("raking")
  expect_true(all(fit$target_check$abs_error < 1e-6))
})

test_that("raking preserves the population margins", {
  fit <- feasible_fit("raking")
  expect_lt(max(abs(fit$margin_check$relative_change)), 1e-6)
})

test_that("raking yields strictly positive weights by construction", {
  fit <- feasible_fit("raking")
  expect_true(all(fit$data$weight_calibrated > 0))
  expect_true(all(is.finite(fit$data$weight_calibrated)))
})

test_that("raking and chi2 reach the same targets via genuinely different weights", {
  rk <- feasible_fit("raking")
  ch <- feasible_fit("chi2")
  # both hit the targets, but the weight distributions differ (different distance)
  expect_true(all(rk$target_check$abs_error < 1e-6))
  expect_false(isTRUE(all.equal(rk$data$weight_calibrated,
                                ch$data$weight_calibrated)))
})

test_that("raking records the distance used in settings", {
  fit <- feasible_fit("raking")
  expect_equal(fit$settings$distance, "raking")
})

test_that("raking now supports soft mode (no longer exact-only)", {
  d <- example_rate_data(n = 400, seed = 7)
  fit <- calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "sex",
                              targets = make_rate_targets(groups = list(sex = c(M = 0.7, F = 0.65))),
                              mode = "soft", distance = "raking")
  expect_s3_class(fit, "pass_rate_calibration")
  expect_equal(fit$settings$mode, "soft")
})

logit_fit <- function(lower = 0.25, upper = 4) {
  d <- example_rate_data(n = 1200, seed = 7)
  calibrate_pass_rates(
    d, outcome = "qualified", weight = "initial_weight",
    group_vars = c("sex", "residence"),
    targets = make_rate_targets(groups = list(sex = c(M = 0.70, F = 0.65))),
    mode = "exact", distance = "logit", lower = lower, upper = upper
  )
}

test_that("logit exact hits the targets and preserves the margins", {
  fit <- logit_fit()
  expect_true(all(fit$target_check$abs_error < 1e-6))
  expect_lt(max(abs(fit$margin_check$relative_change)), 1e-6)
})

test_that("logit keeps every multiplier strictly inside (lower, upper)", {
  fit <- logit_fit(lower = 0.5, upper = 2)
  g <- fit$cell_weights$.multiplier
  expect_true(all(g > 0.5))
  expect_true(all(g < 2))
})

test_that("logit records the distance in settings", {
  expect_equal(logit_fit()$settings$distance, "logit")
})

test_that("logit now supports soft mode (no longer exact-only)", {
  d <- example_rate_data(n = 400, seed = 7)
  fit <- calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "sex",
                              targets = make_rate_targets(groups = list(sex = c(M = 0.7, F = 0.65))),
                              mode = "soft", distance = "logit")
  expect_s3_class(fit, "pass_rate_calibration")
  expect_equal(fit$settings$mode, "soft")
})

test_that("calibrate_rates passes distance through to the solver", {
  d <- example_rate_data(n = 1200, seed = 7)
  fit <- calibrate_rates(
    d, outcome = "qualified", weight = "initial_weight",
    groups = list(sex = c(M = 0.70, F = 0.65)),
    mode = "exact", distance = "raking"
  )
  expect_equal(fit$settings$distance, "raking")
  expect_true(all(fit$target_check$abs_error < 1e-6))
})

test_that("raking supports soft mode: margins hard, weights positive, moves toward targets", {
  d <- example_rate_data(n = 1500, seed = 7)
  tg <- make_rate_targets(groups = list(sex = c(M = 0.75, F = 0.62)))
  fit <- calibrate_pass_rates(d, "qualified", "initial_weight",
                              group_vars = c("sex", "residence"), targets = tg,
                              mode = "soft", distance = "raking")
  expect_lt(max(abs(fit$margin_check$relative_change)), 1e-6)  # margins stay hard
  expect_true(all(fit$data$weight_calibrated > 0))
  tc <- fit$target_check
  # soft moves achieved at least as close to target as the initial rate was
  expect_true(all(abs(tc$achieved_rate - tc$target_rate) <=
                    abs(tc$initial_rate - tc$target_rate) + 1e-9))
})

test_that("a stronger lambda makes soft raking match its targets more closely", {
  d <- example_rate_data(n = 1500, seed = 7)
  tg <- make_rate_targets(groups = list(sex = c(M = 0.75, F = 0.62)))
  loose <- calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "sex",
                                targets = tg, mode = "soft", distance = "raking",
                                lambda = 1)
  tight <- calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "sex",
                                targets = tg, mode = "soft", distance = "raking",
                                lambda = 1e6)
  expect_lt(max(tight$target_check$abs_error), max(loose$target_check$abs_error))
})

test_that("soft logit keeps multipliers within bounds and preserves margins", {
  d <- example_rate_data(n = 1500, seed = 7)
  tg <- make_rate_targets(groups = list(sex = c(M = 0.75, F = 0.62)))
  fit <- calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "sex",
                              targets = tg, mode = "soft", distance = "logit",
                              lower = 0.5, upper = 2)
  expect_lt(max(abs(fit$margin_check$relative_change)), 1e-6)
  g <- fit$cell_weights$.multiplier
  expect_true(all(g > 0.5 - 1e-9 & g < 2 + 1e-9))
})

test_that("soft raking returns a solution even when an exact target is unreachable", {
  set.seed(11)
  d <- data.frame(
    qualified = c(rep(1L, 100), stats::rbinom(100, 1, 0.5)),
    initial_weight = rep(1, 200),
    grp = rep(c("allpass", "mixed"), each = 100),
    stringsAsFactors = FALSE
  )
  tg <- make_rate_targets(groups = list(grp = c(allpass = 0.5, mixed = 0.5)))
  fit <- calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "grp",
                              targets = tg, mode = "soft", distance = "raking")
  expect_s3_class(fit, "pass_rate_calibration")
  expect_true(all(fit$data$weight_calibrated > 0))
})

test_that("raking reports a clear error when exact targets are infeasible", {
  # An all-pass group cannot reach a rate below 1, so an exact target of 0.5 is
  # infeasible and the dual iteration cannot converge.
  set.seed(11)
  d <- data.frame(
    qualified = c(rep(1L, 100), stats::rbinom(100, 1, 0.5)),
    initial_weight = rep(1, 200),
    grp = rep(c("allpass", "mixed"), each = 100),
    stringsAsFactors = FALSE
  )
  targets <- make_rate_targets(groups = list(grp = c(allpass = 0.5, mixed = 0.5)))
  expect_error(
    calibrate_pass_rates(d, "qualified", "initial_weight", group_vars = "grp",
                         targets = targets, mode = "exact", distance = "raking"),
    "converge|infeasible|unreachable"
  )
})
