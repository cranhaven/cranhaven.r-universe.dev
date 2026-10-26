# ==============================================================================
# Extended coverage tests for matching_messages.R
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

test_that("use_emoji respects option", {
  skip_on_cran()
  old <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old), add = TRUE)

  options(couplr.emoji = FALSE)
  expect_false(couplr:::use_emoji())
})

test_that("couplr_emoji returns empty string when disabled", {
  skip_on_cran()
  old <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old), add = TRUE)

  options(couplr.emoji = FALSE)
  expect_equal(couplr:::couplr_emoji("error"), "")
})

# ------------------------------------------------------------------------------
# Error message functions
# ------------------------------------------------------------------------------

test_that("err_missing_data stops with message", {
  skip_on_cran()
  expect_error(couplr:::err_missing_data("left"), "No matches made")
})

test_that("err_missing_vars stops with message", {
  skip_on_cran()
  expect_error(couplr:::err_missing_vars(c("x", "y"), "left"), "Missing variables")
})

test_that("err_invalid_param stops with message", {
  skip_on_cran()
  expect_error(couplr:::err_invalid_param("method", "bad", "auto or jv"), "invalid value")
})

test_that("err_no_valid_pairs stops with message", {
  skip_on_cran()
  expect_error(couplr:::err_no_valid_pairs(), "No valid pairs")
  expect_error(couplr:::err_no_valid_pairs("all infinite"), "Reason")
})

# ------------------------------------------------------------------------------
# Warning message functions
# ------------------------------------------------------------------------------

test_that("warn_constant_var issues warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_constant_var("x"), "constant")
})

test_that("warn_many_zeros issues warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_many_zeros(50.5, 100), "50.5%")
})

test_that("warn_extreme_costs issues warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_extreme_costs(10, 100, 10), "highly skewed")
  expect_warning(couplr:::warn_extreme_costs(10, 100, 10, c("var1")), "extreme values")
})

test_that("warn_many_forbidden issues warning for different severities", {
  skip_on_cran()
  # Critical severity (>90%)
  expect_warning(couplr:::warn_many_forbidden(95, 5, 10), "forbidden")
  # Concerning severity (>75%)
  expect_warning(couplr:::warn_many_forbidden(80, 20, 10), "forbidden")
  # Moderate severity
  expect_warning(couplr:::warn_many_forbidden(60, 40, 10), "forbidden")
})

test_that("warn_constant_distance issues warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_constant_distance(0.5), "identical")
})

test_that("warn_poor_quality issues warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_poor_quality(25, 0.5), "exceed distance")
})

test_that("warn_parallel_unavailable issues warning", {
  skip_on_cran()
  expect_warning(couplr:::warn_parallel_unavailable(), "Parallel processing")
})

# ------------------------------------------------------------------------------
# Info/success message functions
# ------------------------------------------------------------------------------

test_that("couplr_inform outputs message", {
  skip_on_cran()
  expect_message(couplr:::couplr_inform("Test info"), "Test info")
})

test_that("couplr_success outputs message", {
  skip_on_cran()
  expect_message(couplr:::couplr_success("Success!"), "Success!")
})

test_that("info_low_match_rate handles different percentages", {
  skip_on_cran()
  # Very low match rate (<25%) - warning
  expect_warning(couplr:::info_low_match_rate(2, 10, 20), "Only")

  # Moderate match rate (25-50%) - message
  expect_message(couplr:::info_low_match_rate(4, 10, 40), "Moderate")

  # Good match rate (>50%) - no output
  expect_silent(couplr:::info_low_match_rate(6, 10, 60))
})

test_that("success_good_balance outputs message for excellent balance", {
  skip_on_cran()
  expect_message(couplr:::success_good_balance(0.05), "Excellent balance")
  expect_silent(couplr:::success_good_balance(0.15))
})

# ------------------------------------------------------------------------------
# check_cost_distribution
# ------------------------------------------------------------------------------

test_that("check_cost_distribution handles all infinite values", {
  skip_on_cran()
  cost <- matrix(Inf, 3, 3)
  expect_error(couplr:::check_cost_distribution(cost, warn = TRUE), "No valid pairs")

  result <- couplr:::check_cost_distribution(cost, warn = FALSE)
  expect_false(result$valid)
})

test_that("check_cost_distribution warns on many zeros", {
  skip_on_cran()
  cost <- matrix(c(0, 0, 0, 0, 0, 1, 2, 3, 4), 3, 3)
  expect_warning(result <- couplr:::check_cost_distribution(cost, warn = TRUE), "zero")
})

test_that("check_cost_distribution warns on constant distances", {
  skip_on_cran()
  cost <- matrix(1, 3, 3)
  expect_warning(result <- couplr:::check_cost_distribution(cost, warn = TRUE), "identical")
})

test_that("check_cost_distribution warns on extreme costs", {
  skip_on_cran()
  set.seed(123)
  # Create distribution with 99th percentile >> 95th percentile (needs > 10x ratio)
  # 95 values at 1, 5 values at 200 -> p95 ~ 1, p99 ~ 200, ratio = 200
  cost <- matrix(c(rep(1, 95), rep(200, 5)), nrow = 10)
  expect_warning(result <- couplr:::check_cost_distribution(cost, warn = TRUE), "skewed")
})

test_that("check_cost_distribution warns on many forbidden", {
  skip_on_cran()
  cost <- matrix(Inf, 10, 10)
  cost[1, 1:3] <- 1:3  # Only 30 valid pairs
  expect_warning(result <- couplr:::check_cost_distribution(cost, warn = TRUE), "forbidden")
})

# ------------------------------------------------------------------------------
# diagnose_distance_matrix
# ------------------------------------------------------------------------------

test_that("diagnose_distance_matrix runs without data", {
  skip_on_cran()
  cost <- matrix(runif(9), 3, 3)
  result <- diagnose_distance_matrix(cost, warn = FALSE)
  expect_type(result, "list")
  expect_true("distribution" %in% names(result))
  expect_true("quality" %in% names(result))
})

test_that("diagnose_distance_matrix detects constant variables", {
  skip_on_cran()
  cost <- matrix(runif(9), 3, 3)
  left <- data.frame(x = c(1, 1, 1), y = 1:3)
  right <- data.frame(x = c(2, 2, 2), y = 4:6)

  expect_warning(
    result <- diagnose_distance_matrix(cost, left, right, vars = c("x", "y"), warn = TRUE),
    "constant"
  )
  expect_true("x" %in% result$problem_variables)
})

test_that("diagnose_distance_matrix detects scale differences", {
  skip_on_cran()
  cost <- matrix(runif(9), 3, 3)
  left <- data.frame(x = c(1, 2, 3))  # range = 2
  right <- data.frame(x = c(1, 100, 1000))  # range = 999

  result <- diagnose_distance_matrix(cost, left, right, vars = "x", warn = FALSE)
  expect_true(length(result$variable_issues) > 0)
})

test_that("diagnose_distance_matrix generates suggestions", {
  skip_on_cran()
  # Matrix with many forbidden pairs
  cost <- matrix(Inf, 10, 10)
  cost[1:2, 1:2] <- runif(4)

  result <- diagnose_distance_matrix(cost, warn = FALSE)
  expect_true(length(result$suggestions) > 0)
})

test_that("diagnose_distance_matrix assesses quality", {
  skip_on_cran()
  # Good quality
  cost <- matrix(runif(100), 10, 10)
  result <- diagnose_distance_matrix(cost, warn = FALSE)
  expect_equal(result$quality, "good")

  # Fair quality (many forbidden)
  cost <- matrix(Inf, 10, 10)
  cost[1:5, 1:5] <- runif(25)
  result <- diagnose_distance_matrix(cost, warn = FALSE)
  expect_true(result$quality %in% c("fair", "poor"))
})
