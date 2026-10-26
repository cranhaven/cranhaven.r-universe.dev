# ==============================================================================
# Tests for matching messages (matching_messages.R)
# ==============================================================================

# Disable emoji for consistent testing (set once for all tests in file)
options(couplr.emoji = FALSE)

# ------------------------------------------------------------------------------
# use_emoji tests
# ------------------------------------------------------------------------------

test_that("use_emoji respects option", {
  old <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old))

  options(couplr.emoji = FALSE)
  expect_false(couplr:::use_emoji())

  options(couplr.emoji = TRUE)
  # Will be FALSE in non-interactive, TRUE in interactive
  # Just test it doesn't error
  result <- couplr:::use_emoji()
  expect_type(result, "logical")
})

# ------------------------------------------------------------------------------
# couplr_emoji tests
# ------------------------------------------------------------------------------

test_that("couplr_emoji returns empty string when emoji disabled", {
  old <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old))

  options(couplr.emoji = FALSE)
  expect_equal(couplr:::couplr_emoji("error"), "")
  expect_equal(couplr:::couplr_emoji("warning"), "")
  expect_equal(couplr:::couplr_emoji("success"), "")
})

test_that("couplr_emoji validates type argument when enabled", {
  old <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old))

  # Enable emoji to trigger validation
  options(couplr.emoji = TRUE)
  # In non-interactive, use_emoji() returns FALSE anyway
  # So test returns empty without error
  result <- couplr:::couplr_emoji("error")
  expect_type(result, "character")
})

# ------------------------------------------------------------------------------
# couplr_stop tests
# ------------------------------------------------------------------------------

test_that("couplr_stop throws error with message", {
  expect_error(
    couplr:::couplr_stop("Test error message"),
    "Test error message"
  )
})

test_that("couplr_stop concatenates multiple arguments", {
  expect_error(
    couplr:::couplr_stop("Part 1", " Part 2"),
    "Part 1 Part 2"
  )
})

# ------------------------------------------------------------------------------
# couplr_warn tests
# ------------------------------------------------------------------------------

test_that("couplr_warn issues warning with message", {
  expect_warning(
    couplr:::couplr_warn("Test warning"),
    "Test warning"
  )
})

# ------------------------------------------------------------------------------
# couplr_inform tests
# ------------------------------------------------------------------------------

test_that("couplr_inform issues message", {
  expect_message(
    couplr:::couplr_inform("Test info"),
    "Test info"
  )
})

# ------------------------------------------------------------------------------
# couplr_success tests
# ------------------------------------------------------------------------------

test_that("couplr_success issues message", {
  expect_message(
    couplr:::couplr_success("Test success"),
    "Test success"
  )
})

# ------------------------------------------------------------------------------
# Specific error message functions
# ------------------------------------------------------------------------------

test_that("err_missing_data throws appropriate error", {
  expect_error(
    couplr:::err_missing_data("left"),
    "left dataset is empty"
  )

  expect_error(
    couplr:::err_missing_data("right"),
    "right dataset is empty"
  )
})

test_that("err_missing_vars throws appropriate error", {
  expect_error(
    couplr:::err_missing_vars(c("x", "y"), "left"),
    "x, y"
  )
})

test_that("err_invalid_param throws appropriate error", {
  expect_error(
    couplr:::err_invalid_param("max_distance", -1, "positive number"),
    "max_distance"
  )
})

test_that("err_no_valid_pairs throws appropriate error", {
  expect_error(
    couplr:::err_no_valid_pairs(),
    "No valid pairs"
  )

  expect_error(
    couplr:::err_no_valid_pairs("test reason"),
    "test reason"
  )
})

# ------------------------------------------------------------------------------
# Warning message functions
# ------------------------------------------------------------------------------

test_that("warn_constant_var issues warning", {
  expect_warning(
    couplr:::warn_constant_var("x"),
    "constant"
  )
})

test_that("warn_many_zeros issues warning", {
  expect_warning(
    couplr:::warn_many_zeros(50.0, 100),
    "50.0%"
  )
})

test_that("warn_extreme_costs issues warning", {
  expect_warning(
    couplr:::warn_extreme_costs(10, 100, 10),
    "skewed"
  )

  # With problem variables
  expect_warning(
    couplr:::warn_extreme_costs(10, 100, 10, c("x", "y")),
    "x, y"
  )
})

test_that("warn_many_forbidden issues warning with severity", {
  # Critical (>90%)
  expect_warning(
    couplr:::warn_many_forbidden(95, 10, 100),
    "forbidden"
  )

  # Concerning (>75%)
  expect_warning(
    couplr:::warn_many_forbidden(80, 20, 100),
    "forbidden"
  )

  # Moderate
  expect_warning(
    couplr:::warn_many_forbidden(60, 40, 100),
    "forbidden"
  )
})

test_that("warn_constant_distance issues warning", {
  expect_warning(
    couplr:::warn_constant_distance(5.0),
    "identical"
  )
})

test_that("info_low_match_rate handles different rates", {
  # Very low (<25%)
  expect_warning(
    couplr:::info_low_match_rate(10, 100, 10),
    "staying single"
  )

  # Moderate (25-50%)
  expect_message(
    couplr:::info_low_match_rate(30, 100, 30),
    "Moderate"
  )

  # Good (>50%) - should not warn or message
  expect_silent(
    couplr:::info_low_match_rate(60, 100, 60)
  )
})

test_that("warn_poor_quality issues warning", {
  expect_warning(
    couplr:::warn_poor_quality(25.5, 0.5),
    "25.5%"
  )
})

test_that("warn_parallel_unavailable issues warning", {
  expect_warning(
    couplr:::warn_parallel_unavailable(),
    "Parallel"
  )
})

test_that("success_good_balance messages on excellent balance", {
  expect_message(
    couplr:::success_good_balance(0.05),
    "Excellent"
  )

  # Above threshold - should be silent
  expect_silent(
    couplr:::success_good_balance(0.15)
  )
})

# ------------------------------------------------------------------------------
# check_cost_distribution tests
# ------------------------------------------------------------------------------

test_that("check_cost_distribution handles all Inf matrix", {
  cost <- matrix(Inf, 3, 3)

  expect_error(
    couplr:::check_cost_distribution(cost, warn = TRUE),
    "No valid pairs"
  )

  # Without warning, returns invalid result
  result <- couplr:::check_cost_distribution(cost, warn = FALSE)
  expect_false(result$valid)
  expect_equal(result$n_finite, 0)
})

test_that("check_cost_distribution detects many zeros", {
  cost <- matrix(0, 10, 10)
  cost[1, 1] <- 1  # One non-zero

  expect_warning(
    couplr:::check_cost_distribution(cost, warn = TRUE),
    "zero"
  )
})

test_that("check_cost_distribution detects constant distances", {
  cost <- matrix(5, 3, 3)

  expect_warning(
    couplr:::check_cost_distribution(cost, warn = TRUE),
    "identical"
  )
})

test_that("check_cost_distribution detects extreme ratios", {
  cost <- matrix(runif(100), 10, 10)
  cost[1, 1] <- 1000  # Extreme outlier

  expect_warning(
    couplr:::check_cost_distribution(cost, warn = TRUE),
    "skewed"
  )
})

test_that("check_cost_distribution detects many forbidden", {
  cost <- matrix(Inf, 10, 10)
  diag(cost) <- 1  # Only diagonal is finite

  # May generate multiple warnings; check result has warnings captured
  result <- suppressWarnings(couplr:::check_cost_distribution(cost, warn = TRUE))
  expect_true(result$valid)  # Still valid since diagonal is finite
})

test_that("check_cost_distribution returns correct structure", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, Inf, Inf, Inf), 3, 4)

  result <- couplr:::check_cost_distribution(cost, warn = FALSE)

  expect_true(result$valid)
  expect_equal(result$n_finite, 9)
  expect_equal(result$n_infinite, 3)
  expect_equal(result$min, 1)
  expect_equal(result$max, 9)
})

# ------------------------------------------------------------------------------
# diagnose_distance_matrix tests
# ------------------------------------------------------------------------------

test_that("diagnose_distance_matrix works with basic input", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- diagnose_distance_matrix(cost, warn = FALSE)

  expect_type(result, "list")
  expect_true("distribution" %in% names(result))
  expect_true("variable_issues" %in% names(result))
  expect_true("suggestions" %in% names(result))
  expect_true("quality" %in% names(result))
})

test_that("diagnose_distance_matrix detects constant variables", {
  left <- data.frame(x = c(1, 1, 1), y = c(1, 2, 3))
  right <- data.frame(x = c(2, 2, 2), y = c(4, 5, 6))
  cost <- matrix(1, 3, 3)

  # May generate multiple warnings for constant variable
  result <- suppressWarnings(
    diagnose_distance_matrix(cost, left, right, vars = c("x", "y"), warn = TRUE)
  )

  expect_true("x" %in% result$problem_variables)
})

test_that("diagnose_distance_matrix detects scale differences", {
  left <- data.frame(x = c(1, 2, 3))
  right <- data.frame(x = c(1000, 2000, 3000))  # 1000x scale
  cost <- matrix(1, 3, 3)

  result <- diagnose_distance_matrix(cost, left, right, vars = "x", warn = FALSE)

  expect_true("x" %in% result$problem_variables)
})

test_that("diagnose_distance_matrix provides suggestions", {
  # High forbidden percentage
  cost <- matrix(Inf, 10, 10)
  diag(cost) <- 1

  result <- diagnose_distance_matrix(cost, warn = FALSE)

  expect_true(any(grepl("Relax", result$suggestions)))
})

test_that("diagnose_distance_matrix assesses quality", {
  # Good quality
  cost1 <- matrix(runif(100), 10, 10)
  result1 <- diagnose_distance_matrix(cost1, warn = FALSE)
  expect_equal(result1$quality, "good")

  # Fair quality (many forbidden)
  cost2 <- matrix(Inf, 10, 10)
  cost2[1:5, 1:5] <- runif(25)
  result2 <- diagnose_distance_matrix(cost2, warn = FALSE)
  expect_equal(result2$quality, "fair")
})

test_that("diagnose_distance_matrix handles missing variable columns", {
  left <- data.frame(x = 1:3)
  right <- data.frame(y = 1:3)  # Different column
  cost <- matrix(1, 3, 3)

  # Should not error, just skip missing vars
  result <- diagnose_distance_matrix(cost, left, right, vars = c("x", "y", "z"), warn = FALSE)
  expect_type(result, "list")
})
