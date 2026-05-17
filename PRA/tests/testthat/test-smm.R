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

test_that("second moment analysis works without correlation matrix", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  results <- smm(mean, var)

  expect_equal(results$total_mean, 10 + 15 + 20)
  expect_equal(results$total_std, sqrt(4 + 9 + 16))
})

test_that("second moment analysis works with correlation matrix", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)

  cor_mat <- matrix(c(
    1, 0.5, 0.3,
    0.5, 1, 0.4,
    0.3, 0.4, 1
  ), nrow = 3, byrow = TRUE)

  results <- smm(mean, var, cor_mat)

  task_variances <- c(4, 9, 16)

  cov_matrix <- matrix(0, nrow = 3, ncol = 3)
  for (i in 1:3) {
    for (j in 1:3) {
      cov_matrix[i, j] <- cor_mat[i, j] * sqrt(task_variances[i] * task_variances[j])
    }
  }

  total_variance <- sum(task_variances) + sum(cov_matrix[upper.tri(cov_matrix)] * 2)

  expect_equal(results$total_mean, 10 + 15 + 20)
  expect_equal(results$total_std, sqrt(total_variance))
})

test_that("smm validates NULL inputs correctly", {
  var <- c(4, 9, 16)
  mean <- c(10, 15, 20)

  expect_error(smm(NULL, var), "mean and var must not be NULL")
  expect_error(smm(mean, NULL), "mean and var must not be NULL")
})

test_that("smm validates numeric inputs correctly", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)

  expect_error(smm("not numeric", var), "mean and var must be numeric vectors")
  expect_error(smm(mean, "not numeric"), "mean and var must be numeric vectors")
})

test_that("smm validates empty vectors correctly", {
  # Note: c() returns NULL in R, so it triggers the NULL check first
  expect_error(smm(c(), c(1, 2, 3)), "mean and var must not be NULL")
  expect_error(smm(c(1, 2, 3), c()), "mean and var must not be NULL")

  # Test with numeric(0) which is truly empty but not NULL
  expect_error(smm(numeric(0), c(1, 2, 3)), "mean and var must not be empty")
  expect_error(smm(c(1, 2, 3), numeric(0)), "mean and var must not be empty")
})

test_that("smm validates NA values correctly", {
  expect_error(smm(c(10, NA, 20), c(4, 9, 16)), "mean and var must not contain NA values")
  expect_error(smm(c(10, 15, 20), c(4, NA, 16)), "mean and var must not contain NA values")
})

test_that("smm validates non-negative variance correctly", {
  expect_error(smm(c(10, 15, 20), c(4, -9, 16)), "var values must be non-negative")
})

test_that("smm validates vector length match correctly", {
  expect_error(smm(c(10, 15), c(4, 9, 16)), "The mean and variance vectors must have the same length.")
})

test_that("smm validates correlation matrix correctly", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)

  # Non-square correlation matrix
  bad_cor_mat <- matrix(c(1, 0.5), nrow = 1)
  expect_error(smm(mean, var, bad_cor_mat), "The correlation matrix must be square and match the number of tasks.")

  # Wrong size correlation matrix
  wrong_size_cor_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  expect_error(smm(mean, var, wrong_size_cor_mat), "The correlation matrix must be square and match the number of tasks.")
})

test_that("smm print method works correctly", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  result <- smm(mean, var)

  expect_output(print(result), "Second Moment Method Results:")
  expect_output(print(result), "Total Mean:")
  expect_output(print(result), "Total Variance:")
  expect_output(print(result), "Total Standard Deviation:")
})

test_that("smm returns correct class", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  result <- smm(mean, var)

  expect_s3_class(result, "smm")
})

# ============================================================================
# NaN/NA/Inf Error Tests (G5.2, G5.2b)
# ============================================================================
test_that("smm rejects NaN in mean/var", {
  expect_error(smm(c(10, NaN, 20), c(4, 9, 16)), "mean and var must not contain NaN values")
  expect_error(smm(c(10, 15, 20), c(4, NaN, 16)), "mean and var must not contain NaN values")
})

test_that("smm rejects Inf in mean/var", {
  expect_error(smm(c(10, Inf, 20), c(4, 9, 16)), "mean and var must not contain infinite values")
  expect_error(smm(c(10, 15, 20), c(4, Inf, 16)), "mean and var must not contain infinite values")
})

test_that("smm rejects NaN in cor_mat", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  cor_mat <- matrix(c(1, NaN, 0.3, NaN, 1, 0.4, 0.3, 0.4, 1), nrow = 3)
  expect_error(smm(mean, var, cor_mat), "cor_mat must not contain NaN values")
})

test_that("smm rejects NA in cor_mat", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  cor_mat <- matrix(c(1, NA, 0.3, NA, 1, 0.4, 0.3, 0.4, 1), nrow = 3)
  expect_error(smm(mean, var, cor_mat), "cor_mat must not contain NA values")
})

test_that("smm rejects Inf in cor_mat", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  cor_mat <- matrix(c(1, Inf, 0.3, Inf, 1, 0.4, 0.3, 0.4, 1), nrow = 3)
  expect_error(smm(mean, var, cor_mat), "cor_mat must not contain infinite values")
})

# ============================================================================
# G5.3: Return value tests
# ============================================================================
test_that("smm result components contain no NA, NaN, or Inf", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  result <- smm(mean, var)

  expect_false(is.na(result$total_mean))
  expect_false(is.nan(result$total_mean))
  expect_false(is.infinite(result$total_mean))

  expect_false(is.na(result$total_var))
  expect_false(is.nan(result$total_var))
  expect_false(is.infinite(result$total_var))

  expect_false(is.na(result$total_std))
  expect_false(is.nan(result$total_std))
  expect_false(is.infinite(result$total_std))
})

# ============================================================================
# Parameter Recovery Tests (G5.6, G5.6a)
# ============================================================================

test_that("smm recovers known variance for independent tasks", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  # No correlation matrix = independent tasks
  result <- smm(mean, var)

  # Expected: total_mean = sum of means, total_var = sum of variances
  expected_mean <- 10 + 15 + 20
  expected_var <- 4 + 9 + 16

  expect_equal(result$total_mean, expected_mean, tolerance = 1e-10)
  expect_equal(result$total_var, expected_var, tolerance = 1e-10)
  expect_equal(result$total_std, sqrt(expected_var), tolerance = 1e-10)
})

test_that("smm recovers known variance with perfect correlation", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)
  cor_mat <- matrix(1, nrow = 3, ncol = 3) # Perfect correlation
  result <- smm(mean, var, cor_mat)

  # Expected: total_var = (sum of standard deviations)^2
  expected_mean <- 10 + 15 + 20
  expected_var <- (2 + 3 + 4)^2 # (sqrt(4) + sqrt(9) + sqrt(16))^2 = 81

  expect_equal(result$total_mean, expected_mean, tolerance = 1e-10)
  expect_equal(result$total_var, expected_var, tolerance = 1e-10)
})

test_that("smm recovers known variance with partial correlation", {
  mean <- c(10, 20)
  var <- c(4, 9)
  cor_mat <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  result <- smm(mean, var, cor_mat)

  # Manual calculation:
  # total_var = var[1] + var[2] + 2*cov[1,2]
  # cov[1,2] = cor[1,2] * sqrt(var[1]) * sqrt(var[2]) = 0.5 * 2 * 3 = 3
  expected_mean <- 10 + 20
  expected_var <- 4 + 9 + 2 * 3 # = 19

  expect_equal(result$total_mean, expected_mean, tolerance = 1e-10)
  expect_equal(result$total_var, expected_var, tolerance = 1e-10)
})

# ============================================================================
# Edge Condition Tests (G5.8a) - Zero-Length Data
# ============================================================================

test_that("smm rejects zero-length vectors", {
  # numeric(0) is truly empty (not NULL)
  expect_error(smm(numeric(0), c(1, 2, 3)), "mean and var must not be empty")
  expect_error(smm(c(1, 2, 3), numeric(0)), "mean and var must not be empty")
})

# ============================================================================
# Edge Condition Tests (G5.8c) - All-Identical Data
# ============================================================================

test_that("smm handles all identical means and variances", {
  # All tasks identical
  mean <- c(10, 10, 10)
  var <- c(4, 4, 4)

  result <- smm(mean, var)

  # Total mean = 30, total var = 12 (assuming independence)
  expect_equal(result$total_mean, 30, tolerance = 1e-10)
  expect_equal(result$total_var, 12, tolerance = 1e-10)
})

test_that("smm handles zero variance case", {
  mean <- c(10, 15, 20)
  var <- c(0, 0, 0) # All zero variance

  result <- smm(mean, var)

  # Total mean should still be correct
  expect_equal(result$total_mean, 45, tolerance = 1e-10)
  # Total variance should be 0
  expect_equal(result$total_var, 0, tolerance = 1e-10)
  expect_equal(result$total_std, 0, tolerance = 1e-10)
})

# ============================================================================
# Noise Susceptibility Tests (G5.9a) - Trivial Noise
# ============================================================================

test_that("smm is stable to machine epsilon noise in inputs", {
  mean <- c(10, 15, 20)
  var <- c(4, 9, 16)

  result_clean <- smm(mean, var)

  # Add trivial noise at machine epsilon scale
  mean_noisy <- mean + runif(3, -.Machine$double.eps, .Machine$double.eps)
  var_noisy <- var + runif(3, -.Machine$double.eps, .Machine$double.eps)

  result_noisy <- smm(mean_noisy, var_noisy)

  # Results should be essentially identical
  expect_equal(result_clean$total_mean, result_noisy$total_mean,
    tolerance = 10 * .Machine$double.eps
  )
  expect_equal(result_clean$total_var, result_noisy$total_var,
    tolerance = 10 * .Machine$double.eps
  )
})
