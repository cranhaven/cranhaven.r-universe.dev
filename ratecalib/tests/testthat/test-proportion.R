# Tests for generalized proportion targets: the share of an arbitrary
# categorical variable's value (not just the 0/1 outcome). A proportion target
# separates the GROUP (variable/level, the mask) from the MEASURED quantity
# (value_var/value, "share of value_var == value"). The legacy pass rate is the
# special case value_var = outcome, value = 1, which must stay byte-identical.

skip_if_not_installed("osqp")

mk_data <- function(n = 2000, seed = 7) {
  set.seed(seed)
  region <- sample(c("north", "south"), n, TRUE)
  qualified <- stats::rbinom(n, 1, 0.6)
  grade <- sample(c("A", "B", "C"), n, TRUE)   # categorical, used as value_var
  code12 <- sample(c(1L, 2L), n, TRUE)         # 1/2-coded
  w <- stats::runif(n, 0.5, 2)
  data.frame(region, qualified, grade, code12, w, stringsAsFactors = FALSE)
}

test_that("an exact overall proportion of a categorical value is met", {
  d <- mk_data()
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = 0.40,
                   statistic = "proportion", value_var = "grade", value = "A",
                   stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact", lower = 0.1, upper = 10)
  w <- fit$data$weight_calibrated
  expect_equal(sum(w * (d$grade == "A")) / sum(w), 0.40, tolerance = 1e-5)
})

test_that("a proportion target within a group is met for that group", {
  d <- mk_data()
  tg <- data.frame(variable = "region", level = "north", target_rate = 0.45,
                   statistic = "proportion", value_var = "grade", value = "B",
                   stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact", lower = 0.1, upper = 10)
  w <- fit$data$weight_calibrated
  sub <- d$region == "north"
  expect_equal(sum(w[sub] * (d$grade[sub] == "B")) / sum(w[sub]), 0.45,
               tolerance = 1e-5)
})

test_that("proportion of a 1/2-coded value is a share, not a mean", {
  d <- mk_data()
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = 0.55,
                   statistic = "proportion", value_var = "code12", value = "1",
                   stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact", lower = 0.1, upper = 10)
  w <- fit$data$weight_calibrated
  share_of_1 <- sum(w * (d$code12 == 1)) / sum(w)
  expect_equal(share_of_1, 0.55, tolerance = 1e-5)  # share in [0,1], not the mean (~1.45)
})

test_that("an explicit value_var = outcome, value = 1 matches the legacy pass rate", {
  d <- mk_data()
  legacy <- calibrate_pass_rates(
    d, "qualified", "w", group_vars = "region",
    targets = make_rate_targets(groups = list(region = c(north = 0.62, south = 0.58))),
    mode = "exact")
  explicit <- data.frame(variable = "region", level = c("north", "south"),
                         target_rate = c(0.62, 0.58), statistic = "proportion",
                         value_var = "qualified", value = "1", stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = explicit, mode = "exact")
  expect_equal(fit$data$weight_calibrated, legacy$data$weight_calibrated)
})

test_that("a proportion target naming a non-existent value_var errors", {
  d <- mk_data()
  tg <- data.frame(variable = ".overall", level = ".all", target_rate = 0.4,
                   statistic = "proportion", value_var = "nope", value = "x",
                   stringsAsFactors = FALSE)
  expect_error(
    calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                         targets = tg, mode = "exact"),
    "value_var|nope"
  )
})

test_that("two proportion targets on different values of one variable coexist", {
  d <- mk_data()
  tg <- data.frame(
    variable = c(".overall", ".overall"), level = c(".all", ".all"),
    target_rate = c(0.40, 0.35), statistic = "proportion",
    value_var = "grade", value = c("A", "B"), stringsAsFactors = FALSE)
  fit <- calibrate_pass_rates(d, "qualified", "w", group_vars = "region",
                              targets = tg, mode = "exact", lower = 0.05, upper = 20)
  w <- fit$data$weight_calibrated
  expect_equal(sum(w * (d$grade == "A")) / sum(w), 0.40, tolerance = 1e-5)
  expect_equal(sum(w * (d$grade == "B")) / sum(w), 0.35, tolerance = 1e-5)
})
