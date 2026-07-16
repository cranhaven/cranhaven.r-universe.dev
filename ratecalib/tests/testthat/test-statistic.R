# Tests for generalized target statistics: mean and total of a numeric variable
# (the continuous-variable part of the roadmap's statistic generalization).
# Targets carry optional columns `statistic` ("proportion"/"mean"/"total") and
# `value_var`. Proportion (the outcome rate) remains the default and unchanged.

skip_if_not_installed("osqp")

mk_data <- function(n = 2000, seed = 7) {
  set.seed(seed)
  region <- sample(c("north", "south"), n, TRUE)
  qualified <- stats::rbinom(n, 1, 0.6)
  # income varies with qualified so that reweighting within fixed region/overall
  # margins can actually move the income mean/total (otherwise the targets are
  # genuinely infeasible).
  income <- round(stats::rlnorm(n, log(40000) + 0.5 * qualified, 0.3))
  code12 <- 1L + qualified  # 1/2-coded (1 or 2), correlated with qualified
  w <- stats::runif(n, 0.5, 2)
  data.frame(region, qualified, income, code12, w, stringsAsFactors = FALSE)
}

test_that("make_rate_targets builds mean/total rows alongside proportion targets", {
  tg <- make_rate_targets(
    overall = 0.7,
    means = data.frame(variable = ".overall", level = ".all",
                       value_var = "income", target = 50000,
                       stringsAsFactors = FALSE),
    totals = data.frame(variable = "region", level = "north",
                        value_var = "income", target = 1.2e8,
                        stringsAsFactors = FALSE)
  )
  expect_true(all(c("statistic", "value_var") %in% names(tg)))
  # proportion rows keep the legacy meaning
  ov <- tg[tg$variable == ".overall" & tg$statistic == "proportion", ]
  expect_equal(ov$target_rate, 0.7)
  mn <- tg[tg$statistic == "mean", ]
  expect_equal(mn$value_var, "income"); expect_equal(mn$target_rate, 50000)
  tt <- tg[tg$statistic == "total", ]
  expect_equal(tt$variable, "region"); expect_equal(tt$target_rate, 1.2e8)
})

test_that("make_rate_targets keeps its legacy schema when no means/totals given", {
  tg <- make_rate_targets(groups = list(sex = c(M = 0.7, F = 0.6)))
  expect_false("statistic" %in% names(tg))
})

test_that("targets without a statistic column behave exactly as before (regression)", {
  d <- mk_data()
  base_tg <- make_rate_targets(groups = list(region = c(north = 0.62, south = 0.58)))
  fit_base <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                                   targets = base_tg, mode = "exact")
  # Same targets, but with an explicit statistic = "proportion" column added.
  tg2 <- base_tg
  tg2$statistic <- "proportion"
  fit2 <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                               targets = tg2, mode = "exact")
  expect_equal(fit2$data$weight_calibrated, fit_base$data$weight_calibrated)
})

test_that("an exact overall mean target is met for a continuous variable", {
  d <- mk_data()
  init_mean <- sum(d$w * d$income) / sum(d$w)
  target_mean <- init_mean * 1.05
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = target_mean,
                   statistic = "mean", value_var = "income", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact",
                              lower = 0.1, upper = 10)
  w <- fit$data$weight_calibrated
  expect_equal(sum(w * d$income) / sum(w), target_mean, tolerance = 1e-4)
})

test_that("an exact group total target is met for a continuous variable", {
  d <- mk_data()
  init_total_north <- sum(d$w[d$region == "north"] * d$income[d$region == "north"])
  target_total <- init_total_north * 1.05
  tg <- data.frame(variable = "region", level = "north", target_rate = target_total,
                   statistic = "total", value_var = "income", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact",
                              lower = 0.1, upper = 10)
  w <- fit$data$weight_calibrated
  sub <- d$region == "north"
  expect_equal(sum(w[sub] * d$income[sub]), target_total, tolerance = 1)
})

test_that("a mean target on 1/2-coded data is a mean, not a proportion", {
  d <- mk_data()
  init_mean <- sum(d$w * d$code12) / sum(d$w)  # around 1.5, never in [0,1]
  target_mean <- 1.3
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = target_mean,
                   statistic = "mean", value_var = "code12", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact", lower = 0.1, upper = 10)
  w <- fit$data$weight_calibrated
  achieved <- sum(w * d$code12) / sum(w)
  expect_equal(achieved, 1.3, tolerance = 1e-4)
  expect_gt(achieved, 1)  # a proportion would be in [0,1]; a mean of {1,2} is not
})

test_that("a total target is met under the raking dual solver (non-zero rhs path)", {
  d <- mk_data()
  init_total <- sum(d$w * d$income)
  target_total <- init_total * 0.97
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = target_total,
                   statistic = "total", value_var = "income", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact", distance = "raking")
  w <- fit$data$weight_calibrated
  expect_equal(sum(w * d$income), target_total, tolerance = 1)
})

test_that("mean and total targets may exceed 1 without tripping rate validation", {
  d <- mk_data()
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = 50000,
                   statistic = "mean", value_var = "income", stringsAsFactors = FALSE)
  expect_error(
    calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                         targets = tg, mode = "exact", lower = 0.1, upper = 10),
    NA  # should NOT error about target_rate being between 0 and 1
  )
})

test_that("mean targets now work in soft mode: margins hard, moves toward target", {
  d <- mk_data()
  init_mean <- sum(d$w * d$income) / sum(d$w)
  target_mean <- init_mean * 1.05
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = target_mean,
                   statistic = "mean", value_var = "income", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "soft", lower = 0.1, upper = 10)
  expect_lt(max(abs(fit$margin_check$relative_change)), 1e-6)
  w <- fit$data$weight_calibrated
  achieved <- sum(w * d$income) / sum(w)
  expect_lt(abs(achieved - target_mean), abs(init_mean - target_mean))
})

test_that("a stronger lambda drives a soft mean target close to the target", {
  d <- mk_data()
  init_mean <- sum(d$w * d$income) / sum(d$w)
  target_mean <- init_mean * 1.05
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = target_mean,
                   statistic = "mean", value_var = "income", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "soft", lower = 0.1, upper = 10,
                              lambda = 1e7)
  w <- fit$data$weight_calibrated
  achieved <- sum(w * d$income) / sum(w)
  expect_lt(abs(achieved - target_mean) / target_mean, 1e-3)  # within 0.1% relative
})

test_that("a soft total target returns a solution even when exact would be infeasible", {
  d <- mk_data()
  # a wildly large total is unreachable exactly, but soft must still solve
  tg <- data.frame(variable = ".overall", level = ".all",
                   target_rate = sum(d$w * d$income) * 100,
                   statistic = "total", value_var = "income", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "soft", lower = 0.1, upper = 10)
  expect_s3_class(fit, "pass_rate_calibration")
  expect_true(all(fit$data$weight_calibrated > 0))
})

test_that("a mean/total target naming a non-existent value_var errors", {
  d <- mk_data()
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = 5,
                   statistic = "mean", value_var = "nope", stringsAsFactors = FALSE)
  expect_error(
    calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                         targets = tg, mode = "exact"),
    "value_var|nope"
  )
})
