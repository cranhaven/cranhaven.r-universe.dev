test_that("exact mode meets group targets and preserves margins", {
  skip_if_not_installed("osqp")
  set.seed(42)
  n <- 2000
  d <- data.frame(
    y = stats::rbinom(n, 1, 0.6),
    w = stats::runif(n, 0.5, 2),
    g = sample(c("a", "b", "c"), n, TRUE)
  )
  targets <- make_rate_targets(groups = list(g = c(a = 0.60, b = 0.62, c = 0.64)))

  fit <- calibrate_pass_rates(
    d, outcome = "y", weight = "w", group_vars = "g",
    targets = targets, mode = "exact"
  )

  expect_match(fit$solver_status, "solved", ignore.case = TRUE)
  # Exact mode should hit each target essentially exactly.
  expect_lt(max(fit$target_check$abs_error), 1e-5)
  # Population margins must be preserved.
  expect_lt(max(abs(fit$margin_check$relative_change)), 1e-5)
  expect_true(all(fit$data$weight_calibrated > 0))
})

test_that("soft mode moves achieved rates close to targets", {
  skip_if_not_installed("osqp")
  set.seed(7)
  n <- 3000
  d <- data.frame(
    y = stats::rbinom(n, 1, 0.5),
    w = stats::runif(n, 0.5, 2),
    g = sample(c("a", "b"), n, TRUE)
  )
  targets <- make_rate_targets(groups = list(g = c(a = 0.55, b = 0.45)))

  initial_gap <- {
    wr <- function(mask) sum(d$w[mask] * d$y[mask]) / sum(d$w[mask])
    max(abs(wr(d$g == "a") - 0.55), abs(wr(d$g == "b") - 0.45))
  }

  fit <- calibrate_pass_rates(
    d, outcome = "y", weight = "w", group_vars = "g",
    targets = targets, mode = "soft", lambda = 1e6
  )

  # With a large penalty and reachable targets, achieved should be very close.
  expect_lt(max(fit$target_check$abs_error), 0.01)
  # And clearly better than the un-calibrated gap.
  expect_lt(max(fit$target_check$abs_error), initial_gap)
})

test_that("cells hit the lower bound when the target is unreachable within bounds", {
  skip_if_not_installed("osqp")
  # Single cell of passers and failers; raising the overall rate to 0.85 while
  # the failing cell is floored at 0.4x its weight forces a lower-bound hit.
  db <- data.frame(
    y = rep(c(1, 0), each = 500),
    w = rep(1, 1000),
    g = rep("x", 1000)
  )
  tb <- make_rate_targets(overall = 0.85)

  fit <- calibrate_pass_rates(
    db, outcome = "y", weight = "w", group_vars = "g",
    targets = tb, lower = 0.4, upper = 4, mode = "soft", lambda = 1e6
  )

  expect_true(any(fit$cell_weights$.at_lower_bound))
  expect_type(fit$cell_weights$.at_lower_bound, "logical")
  expect_type(fit$cell_weights$.at_upper_bound, "logical")
  expect_gte(
    fit$diagnostics$value[fit$diagnostics$metric == "cells_at_lower_bound"],
    1
  )
})

test_that("exact mode errors when targets are infeasible within bounds", {
  skip_if_not_installed("osqp")
  db <- data.frame(
    y = rep(c(1, 0), each = 500),
    w = rep(1, 1000),
    g = rep("x", 1000)
  )
  tb <- make_rate_targets(overall = 0.85)

  expect_error(
    calibrate_pass_rates(
      db, outcome = "y", weight = "w", group_vars = "g",
      targets = tb, lower = 0.4, upper = 4, mode = "exact"
    ),
    "did not solve"
  )
})
