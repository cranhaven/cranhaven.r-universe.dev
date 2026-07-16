base_data <- function(n = 600) {
  set.seed(11)
  data.frame(
    y = stats::rbinom(n, 1, 0.6),
    w = stats::runif(n, 0.5, 2),
    g = sample(c("a", "b"), n, TRUE),
    stringsAsFactors = FALSE
  )
}
base_targets <- function() make_rate_targets(groups = list(g = c(a = 0.6, b = 0.6)))

test_that("calibrate_pass_rates rejects non-positive weights", {
  d <- base_data()
  d$w[1] <- 0
  expect_error(
    calibrate_pass_rates(d, "y", "w", "g", base_targets()),
    "finite and strictly positive"
  )
})

test_that("calibrate_pass_rates rejects non-binary outcomes", {
  d <- base_data()
  d$y[1] <- 3
  expect_error(
    calibrate_pass_rates(d, "y", "w", "g", base_targets()),
    "only 0 and 1"
  )
})

test_that("calibrate_pass_rates rejects a target variable absent from group_vars", {
  d <- base_data()
  targets <- make_rate_targets(groups = list(other = c(x = 0.5)))
  expect_error(
    calibrate_pass_rates(d, "y", "w", "g", targets),
    "not in group_vars"
  )
})

test_that("calibrate_pass_rates validates bounds and lambda", {
  d <- base_data()
  expect_error(
    calibrate_pass_rates(d, "y", "w", "g", base_targets(), lower = 1.2, upper = 4),
    "lower"
  )
  expect_error(
    calibrate_pass_rates(d, "y", "w", "g", base_targets(), lambda = -1),
    "lambda"
  )
})

test_that("calibrate_pass_rates rejects targets with missing required columns", {
  d <- base_data()
  bad <- data.frame(variable = "g", level = "a")  # no target_rate
  expect_error(
    calibrate_pass_rates(d, "y", "w", "g", bad),
    "missing columns"
  )
})

test_that("make_rate_targets validates inputs", {
  expect_error(make_rate_targets(overall = 1.5), "between 0 and 1")
  expect_error(make_rate_targets(groups = list(c(a = 0.5))), "named list")
  expect_error(
    make_rate_targets(groups = list(g = c(a = 2))),
    "between 0 and 1"
  )
})

test_that("calibrate_rates requires a named groups list", {
  d <- base_data()
  expect_error(
    calibrate_rates(d, "y", "w", groups = list(c(a = 0.5))),
    "named list"
  )
})
