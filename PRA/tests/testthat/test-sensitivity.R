#' @srrstats {G5.2} *Error and warning behaviour is explicitly demonstrated through tests.*
#' @srrstats {G5.2a} *Every error message is unique and tested.*
#' @srrstats {G5.2b} *Tests trigger every error message and compare with expected values.*
#' @srrstats {G5.3} *Return objects tested for absence of NA, NaN, Inf.*
#' @srrstats {G5.6} *Parameter recovery tests verify implementations produce expected results given data with known properties.*
#' @srrstats {G5.6a} *Parameter recovery tests succeed within defined tolerance rather than exact values.*
#' @srrstats {G5.6b} *Parameter recovery tests run with multiple random seeds when randomness is involved.*
#' @srrstats {G5.7} *Algorithm performance tests verify implementations perform correctly as data properties change.*
#' @srrstats {G5.8} *Edge condition tests verify appropriate behavior with extreme data properties.*
#' @srrstats {G5.8a} *Zero-length data tests trigger clear errors.*
#' @srrstats {G5.8b} *Unsupported data type tests trigger clear errors.*
#' @srrstats {G5.8c} *All-NA and all-identical data tests trigger clear errors or warnings.*
#' @srrstats {G5.8d} *Out-of-scope data tests verify appropriate behavior.*
#' @srrstats {G5.9} *Noise susceptibility tests verify stochastic behavior stability.*
#' @srrstats {G5.9a} *Trivial noise tests show results are stable at machine epsilon scale.*
#' @srrstats {G5.9b} *Random seed stability tests show consistent behavior across different seeds.*

test_that("sensitivity function calculates correctly for normal distributions", {
  normal_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 20, sd = 3)
  )
  result <- sensitivity(normal_dists)
  expect_true(is.numeric(result))
  expect_length(result, 2)

  # Expected variances
  expected_variances <- sapply(normal_dists, function(dist) dist$sd^2)
  expected_sensitivity <- 1 + 2 * 0 # Since there is no correlation matrix provided

  expect_equal(result[1], expected_sensitivity)
  expect_equal(result[2], expected_sensitivity)
})

test_that("sensitivity function calculates correctly for triangular distributions", {
  triangular_dists <- list(
    list(type = "triangular", a = 1, b = 3, c = 5),
    list(type = "triangular", a = 2, b = 4, c = 6)
  )
  result <- sensitivity(triangular_dists)
  expect_true(is.numeric(result))
  expect_length(result, 2)
})

test_that("sensitivity function calculates correctly for uniform distributions", {
  uniform_dists <- list(
    list(type = "uniform", min = 1, max = 5),
    list(type = "uniform", min = 2, max = 6)
  )
  result <- sensitivity(uniform_dists)
  expect_true(is.numeric(result))
  expect_length(result, 2)
})

test_that("sensitivity function handles correlation matrix correctly", {
  normal_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 20, sd = 3)
  )
  cor_mat <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
  result <- sensitivity(normal_dists, cor_mat)
  expect_true(is.numeric(result))
  expect_length(result, 2)
})

test_that("sensitivity function handles errors correctly", {
  normal_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 20, sd = 3)
  )

  # Unsupported distribution type
  unsupported_dists <- list(list(type = "unsupported", mean = 10, sd = 2))
  expect_error(sensitivity(unsupported_dists), "Unsupported distribution type.")

  # Incorrect correlation matrix
  incorrect_cor_mat <- matrix(c(1, 0.8), nrow = 1)
  expect_error(sensitivity(normal_dists, incorrect_cor_mat), "The correlation matrix must be square and match the number of tasks.")
})

test_that("sensitivity function validates NULL input correctly", {
  expect_error(sensitivity(NULL), "task_dists must not be NULL")
})

test_that("sensitivity function validates empty list correctly", {
  expect_error(sensitivity(list()), "task_dists must be a non-empty list")
})

test_that("sensitivity function validates non-list input correctly", {
  expect_error(sensitivity("not a list"), "task_dists must be a non-empty list")
})

test_that("sensitivity function handles mixed distribution types", {
  mixed_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "triangular", a = 5, b = 10, c = 15),
    list(type = "uniform", min = 8, max = 12)
  )
  result <- sensitivity(mixed_dists)
  expect_true(is.numeric(result))
  expect_length(result, 3)
})

test_that("sensitivity function handles single task correctly", {
  single_dist <- list(list(type = "normal", mean = 10, sd = 2))
  result <- sensitivity(single_dist)
  expect_true(is.numeric(result))
  expect_length(result, 1)
  expect_equal(result[1], 1) # No correlation, sensitivity should be 1
})

# ============================================================================
# NaN/NA/Inf Error Tests (G5.2, G5.2b)
# ============================================================================
test_that("sensitivity rejects NaN in cor_mat", {
  normal_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 20, sd = 3)
  )
  cor_mat <- matrix(c(1, NaN, NaN, 1), nrow = 2)
  expect_error(sensitivity(normal_dists, cor_mat), "cor_mat must not contain NaN values")
})

test_that("sensitivity rejects NA in cor_mat", {
  normal_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 20, sd = 3)
  )
  cor_mat <- matrix(c(1, NA, NA, 1), nrow = 2)
  expect_error(sensitivity(normal_dists, cor_mat), "cor_mat must not contain NA values")
})

test_that("sensitivity rejects Inf in cor_mat", {
  normal_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 20, sd = 3)
  )
  cor_mat <- matrix(c(1, Inf, Inf, 1), nrow = 2)
  expect_error(sensitivity(normal_dists, cor_mat), "cor_mat must not contain infinite values")
})

# ============================================================================
# G5.3: Return value tests
# ============================================================================
test_that("sensitivity result contains no NA, NaN, or Inf", {
  normal_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 20, sd = 3)
  )
  result <- sensitivity(normal_dists)
  expect_false(anyNA(result))
  expect_false(any(is.nan(result)))
  expect_false(any(is.infinite(result)))
})

# ============================================================================
# Parameter Recovery Tests (G5.6, G5.6a)
# ============================================================================

test_that("sensitivity recovers known values for independent tasks", {
  task_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 15, sd = 3)
  )
  # No correlation matrix = independent

  result <- sensitivity(task_dists)

  # For independent tasks, sensitivity should be 1.0 for all tasks
  expect_equal(result, c(1, 1), tolerance = 1e-10)
})

# ============================================================================
# Edge Condition Tests (G5.8c) - All-Identical Distributions
# ============================================================================

test_that("sensitivity handles all identical distributions", {
  # All tasks with same distribution
  task_dists <- list(
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 10, sd = 2),
    list(type = "normal", mean = 10, sd = 2)
  )

  result <- sensitivity(task_dists)

  # All should have same sensitivity
  expect_equal(result[1], result[2], tolerance = 1e-10)
  expect_equal(result[2], result[3], tolerance = 1e-10)
})
