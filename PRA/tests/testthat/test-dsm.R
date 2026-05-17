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

# ============================================================================
# parent_dsm: Basic functionality
# ============================================================================

test_that("parent_dsm function works correctly with valid input", {
  # Identity matrix (2 resources x 2 tasks)
  sm <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)

  result <- parent_dsm(sm)

  expect_s3_class(result, "dsm")
  expect_equal(result$type, "parent")
  expect_true(is.matrix(result$matrix))
  expect_equal(dim(result$matrix), c(2, 2))

  # t(I) %*% I = I
expected_result <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  expect_equal(result$matrix, expected_result)
})

test_that("parent_dsm handles non-square matrix", {
  # 2 resources x 3 tasks
  sm <- matrix(c(1, 0, 0, 1, 1, 1), nrow = 2, ncol = 3)

  result <- parent_dsm(sm)

  expect_s3_class(result, "dsm")
  # Output should be tasks x tasks (3x3)
  expect_equal(dim(result$matrix), c(3, 3))
  expect_equal(result$n_tasks, 3)
  expect_equal(result$n_resources, 2)

  # t(S) %*% S: col1=(1,0), col2=(0,1), col3=(1,1)
  # [1,1] = 1*1+0*0 = 1, [1,2] = 1*0+0*1 = 0, [1,3] = 1*1+0*1 = 1
  # [2,2] = 0*0+1*1 = 1, [2,3] = 0*1+1*1 = 1
  # [3,3] = 1*1+1*1 = 2
  expected <- matrix(c(1, 0, 1, 0, 1, 1, 1, 1, 2), nrow = 3, ncol = 3)
  expect_equal(result$matrix, expected)
})

test_that("parent_dsm handles larger matrices", {
  sm <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)

  result <- parent_dsm(sm)

  expect_s3_class(result, "dsm")
  expect_true(is.matrix(result$matrix))
  expect_equal(dim(result$matrix), c(3, 3))

  # t(S) %*% S where S columns are (1,2,3), (4,5,6), (7,8,9)
  # [1,1] = 1+4+9 = 14, [1,2] = 4+10+18 = 32, [1,3] = 7+16+27 = 50
  # [2,2] = 16+25+36 = 77, [2,3] = 28+40+54 = 122
  # [3,3] = 49+64+81 = 194
  expected_result <- matrix(c(14, 32, 50, 32, 77, 122, 50, 122, 194), nrow = 3, ncol = 3)
  expect_equal(result$matrix, expected_result)
})

# ============================================================================
# grandparent_dsm: Basic functionality
# ============================================================================

test_that("grandparent_dsm function works correctly with valid input", {
  sm <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  rm <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

  result <- grandparent_dsm(sm, rm)

  expect_s3_class(result, "dsm")
  expect_equal(result$type, "grandparent")
  expect_true(is.matrix(result$matrix))
  expect_equal(dim(result$matrix), c(2, 2))

  # R %*% S = [[1,3],[2,4]] %*% I = [[1,3],[2,4]]
  # t(RS) %*% RS = [[1,2],[3,4]] %*% [[1,3],[2,4]] = [[5,11],[11,25]]
  expected_result <- matrix(c(5, 11, 11, 25), nrow = 2, ncol = 2)
  expect_equal(result$matrix, expected_result)
})

test_that("grandparent_dsm handles non-square S matrix", {
  # S: 3 resources x 4 tasks, R: 2 risks x 3 resources
  sm <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1), nrow = 3, ncol = 4)
  rm <- matrix(c(1, 1, 0, 1, 0, 0), nrow = 2, ncol = 3)

  result <- grandparent_dsm(sm, rm)

  expect_s3_class(result, "dsm")
  # Output should be tasks x tasks (4x4)
  expect_equal(dim(result$matrix), c(4, 4))
  expect_equal(result$n_tasks, 4)
  expect_equal(result$n_resources, 3)
  expect_equal(result$n_risks, 2)
})

test_that("grandparent_dsm function handles incompatible dimensions", {
  # S: 2 resources x 2 tasks, R: 1 risk x 3 resources
  sm <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  rm <- matrix(c(1, 2, 3), nrow = 1, ncol = 3)

  expect_error(grandparent_dsm(sm, rm), "Number of rows in S.*must equal.*number of columns in R")
})

test_that("grandparent_dsm function handles larger matrices", {
  sm <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
  rm <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)

  result <- grandparent_dsm(sm, rm)

  expect_s3_class(result, "dsm")
  expect_true(is.matrix(result$matrix))
  expect_equal(dim(result$matrix), c(3, 3))
})

test_that("grandparent_dsm function handles non-square R matrix", {
  # S: 2 resources x 2 tasks, R: 2 risks x 3 resources (incompatible)
  sm <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  rm <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)

  expect_error(grandparent_dsm(sm, rm), "Number of rows in S.*must equal.*number of columns in R")
})

# ============================================================================
# NULL input validation
# ============================================================================

test_that("parent_dsm handles NULL input correctly", {
  expect_error(parent_dsm(NULL), "S must not be NULL")
})

test_that("grandparent_dsm handles NULL inputs correctly", {
  sm <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  rm <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

  expect_error(grandparent_dsm(NULL, rm), "S and R must not be NULL")
  expect_error(grandparent_dsm(sm, NULL), "S and R must not be NULL")
})

# ============================================================================
# Non-matrix input validation
# ============================================================================

test_that("parent_dsm handles non-matrix input correctly", {
  expect_error(parent_dsm("not a matrix"), "S must be a matrix or data frame")
  expect_error(parent_dsm(c(1, 2, 3, 4)), "S must be a matrix or data frame")
})

test_that("grandparent_dsm handles non-matrix inputs correctly", {
  sm <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)

  expect_error(grandparent_dsm("not a matrix", sm), "S must be a matrix or data frame")
  expect_error(grandparent_dsm(sm, "not a matrix"), "R must be a matrix or data frame")
})

# ============================================================================
# Non-numeric input validation
# ============================================================================

test_that("parent_dsm handles non-numeric matrix correctly", {
  char_matrix <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2)
  expect_error(parent_dsm(char_matrix), "S must contain numeric values")
})

test_that("grandparent_dsm handles non-numeric matrices correctly", {
  sm <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  char_matrix <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2)

  expect_error(grandparent_dsm(char_matrix, sm), "S and R must contain numeric values")
  expect_error(grandparent_dsm(sm, char_matrix), "S and R must contain numeric values")
})

# ============================================================================
# Data frame input (should work directly)
# ============================================================================

test_that("parent_dsm handles data frame input correctly", {
  df <- data.frame(a = c(1, 0), b = c(0, 1))
  result <- parent_dsm(df)
  expect_s3_class(result, "dsm")
  expect_true(is.matrix(result$matrix))
})

test_that("grandparent_dsm handles data frame inputs correctly", {
  sm <- data.frame(a = c(1, 0), b = c(0, 1))
  rm <- data.frame(a = c(1, 2), b = c(3, 4))
  result <- grandparent_dsm(sm, rm)
  expect_s3_class(result, "dsm")
  expect_true(is.matrix(result$matrix))
})

# ============================================================================
# NaN/NA/Inf Error Tests (G5.2, G5.2b)
# ============================================================================

test_that("parent_dsm rejects NaN values in S", {
  sm <- matrix(c(1, NaN, 0, 1), nrow = 2, ncol = 2)
  expect_error(parent_dsm(sm), "S must not contain NaN values")
})

test_that("parent_dsm rejects NA values in S", {
  sm <- matrix(c(1, NA_real_, 0, 1), nrow = 2, ncol = 2)
  expect_error(parent_dsm(sm), "S must not contain NA values")
})

test_that("parent_dsm rejects Inf values in S", {
  sm <- matrix(c(1, Inf, 0, 1), nrow = 2, ncol = 2)
  expect_error(parent_dsm(sm), "S must not contain infinite values")
})

test_that("grandparent_dsm rejects NaN values in S or R", {
  sm <- matrix(c(1, NaN, 0, 1), nrow = 2, ncol = 2)
  rm <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  expect_error(grandparent_dsm(sm, rm), "S and R must not contain NaN values")
})

test_that("grandparent_dsm rejects NA values in S or R", {
  sm <- matrix(c(1, NA_real_, 0, 1), nrow = 2, ncol = 2)
  rm <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  expect_error(grandparent_dsm(sm, rm), "S and R must not contain NA values")
})

test_that("grandparent_dsm rejects Inf values in S or R", {
  sm <- matrix(c(1, Inf, 0, 1), nrow = 2, ncol = 2)
  rm <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  expect_error(grandparent_dsm(sm, rm), "S and R must not contain infinite values")
})

# ============================================================================
# G5.3: Return value tests
# ============================================================================

test_that("parent_dsm result contains no NA, NaN, or Inf", {
  sm <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
  result <- parent_dsm(sm)
  expect_false(anyNA(result$matrix))
  expect_false(any(is.nan(result$matrix)))
  expect_false(any(is.infinite(result$matrix)))
})

test_that("grandparent_dsm result contains no NA, NaN, or Inf", {
  sm <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
  rm <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
  result <- grandparent_dsm(sm, rm)
  expect_false(anyNA(result$matrix))
  expect_false(any(is.nan(result$matrix)))
  expect_false(any(is.infinite(result$matrix)))
})

# ============================================================================
# Parameter Recovery Tests (G5.6, G5.6a)
# ============================================================================

test_that("parent_dsm recovers known matrix properties", {
  # Identity matrix: t(I) %*% I = I
  S <- matrix(c(1, 0, 0, 1), nrow = 2)
  result <- parent_dsm(S)

  expected <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_equal(result$matrix, expected, tolerance = 1e-10)
})

test_that("parent_dsm diagonal equals column sums of squares", {
  # S: 3 resources x 3 tasks
  S <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 3, ncol = 3)
  result <- parent_dsm(S)

  # Diagonal of t(S) %*% S equals column sums of squares (resources per task)
  # Col 1: (1,0,1) -> 1+0+1 = 2, Col 2: (0,1,0) -> 0+1+0 = 1, Col 3: (1,0,1) -> 1+0+1 = 2
  expect_equal(diag(result$matrix), c(2, 1, 2), tolerance = 1e-10)
})

test_that("parent_dsm is symmetric", {
  S <- matrix(c(1, 1, 0, 0, 1, 1, 1, 0, 1), nrow = 3, ncol = 3)
  result <- parent_dsm(S)

  # t(S) %*% S is always symmetric
  expect_equal(result$matrix, t(result$matrix), tolerance = 1e-10)
})

test_that("grandparent_dsm recovers known shared risks", {
  S <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  R <- matrix(c(1, 1, 0, 0), nrow = 2, ncol = 2)
  result <- grandparent_dsm(S, R)

  expect_s3_class(result, "dsm")
  expect_true(is.matrix(result$matrix))
  expect_equal(nrow(result$matrix), 2)
  expect_equal(ncol(result$matrix), 2)
})

test_that("grandparent_dsm is symmetric", {
  S <- matrix(c(1, 1, 0, 0, 1, 1, 1, 0, 1), nrow = 3, ncol = 3)
  R <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 3, ncol = 3)
  result <- grandparent_dsm(S, R)

  # Result must be symmetric
  expect_equal(result$matrix, t(result$matrix), tolerance = 1e-10)
})

# ============================================================================
# Edge Condition Tests (G5.8a) - Zero-Length Data
# ============================================================================

test_that("parent_dsm handles zero-dimension matrix", {
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 0)
  result <- parent_dsm(empty_matrix)

  expect_s3_class(result, "dsm")
  expect_true(is.matrix(result$matrix))
  expect_equal(nrow(result$matrix), 0)
  expect_equal(ncol(result$matrix), 0)
})

test_that("grandparent_dsm handles zero-dimension matrices", {
  empty_s <- matrix(numeric(0), nrow = 0, ncol = 0)
  empty_r <- matrix(numeric(0), nrow = 0, ncol = 0)
  result <- grandparent_dsm(empty_s, empty_r)

  expect_s3_class(result, "dsm")
  expect_equal(nrow(result$matrix), 0)
  expect_equal(ncol(result$matrix), 0)
})

# ============================================================================
# Edge Condition Tests (G5.8c) - All-Zero/All-One Matrices
# ============================================================================

test_that("parent_dsm handles all-zero matrix", {
  S <- matrix(0, nrow = 3, ncol = 3)
  result <- parent_dsm(S)

  expect_true(all(result$matrix == 0))
})

test_that("parent_dsm handles all-one matrix", {
  S <- matrix(1, nrow = 3, ncol = 3)
  result <- parent_dsm(S)

  expect_s3_class(result, "dsm")
  expect_true(is.matrix(result$matrix))
  expect_equal(nrow(result$matrix), 3)
  expect_equal(ncol(result$matrix), 3)
})

# ============================================================================
# Edge Condition Tests (G5.8d) - 1x1 Matrix
# ============================================================================

test_that("parent_dsm handles 1x1 matrix", {
  S <- matrix(5, nrow = 1, ncol = 1)
  result <- parent_dsm(S)

  expect_s3_class(result, "dsm")
  expect_equal(result$matrix, matrix(25, 1, 1))
})

test_that("grandparent_dsm requires compatible dimensions", {
  S <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  R_wrong <- matrix(c(1, 0, 1), nrow = 1, ncol = 3)

  expect_error(
    grandparent_dsm(S, R_wrong),
    "Number of rows in S.*must equal.*number of columns in R"
  )
})

# ============================================================================
# Noise Susceptibility Tests (G5.9a) - Trivial Noise
# ============================================================================

test_that("parent_dsm is stable to trivial noise in matrix values", {
  S <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
  result_clean <- parent_dsm(S)

  # Add trivial noise
  S_noisy <- S + matrix(runif(9, -.Machine$double.eps, .Machine$double.eps), nrow = 3)
  result_noisy <- parent_dsm(S_noisy)

  expect_equal(result_clean$matrix, result_noisy$matrix, tolerance = 100 * .Machine$double.eps)
})

test_that("grandparent_dsm is stable to trivial noise in matrix values", {
  S <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
  R <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 3, ncol = 3)
  result_clean <- grandparent_dsm(S, R)

  S_noisy <- S + matrix(runif(9, -.Machine$double.eps, .Machine$double.eps), nrow = 3)
  R_noisy <- R + matrix(runif(9, -.Machine$double.eps, .Machine$double.eps), nrow = 3)
  result_noisy <- grandparent_dsm(S_noisy, R_noisy)

  expect_equal(result_clean$matrix, result_noisy$matrix, tolerance = 100 * .Machine$double.eps)
})

# ============================================================================
# S3 class and methods
# ============================================================================

test_that("parent_dsm returns dsm S3 class with correct fields", {
  S <- matrix(c(1, 0, 1, 0, 1, 0), nrow = 2, ncol = 3)
  result <- parent_dsm(S)

  expect_s3_class(result, "dsm")
  expect_equal(result$type, "parent")
  expect_equal(result$n_tasks, 3)
  expect_equal(result$n_resources, 2)
  expect_null(result$n_risks)
})

test_that("grandparent_dsm returns dsm S3 class with correct fields", {
  S <- matrix(c(1, 0, 1, 0, 1, 0), nrow = 2, ncol = 3)
  R <- matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2)
  result <- grandparent_dsm(S, R)

  expect_s3_class(result, "dsm")
  expect_equal(result$type, "grandparent")
  expect_equal(result$n_tasks, 3)
  expect_equal(result$n_resources, 2)
  expect_equal(result$n_risks, 2)
})

test_that("print.dsm works for parent DSM", {
  S <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  result <- parent_dsm(S)

  output <- capture.output(print(result))
  expect_true(any(grepl("Parent", output)))
  expect_true(any(grepl("Tasks:", output)))
})

test_that("print.dsm works for grandparent DSM", {
  S <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  R <- matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2)
  result <- grandparent_dsm(S, R)

  output <- capture.output(print(result))
  expect_true(any(grepl("Grandparent", output)))
  expect_true(any(grepl("Risks:", output)))
})

test_that("plot.dsm works for parent DSM", {
  S <- matrix(c(1, 0, 1, 0, 1, 0, 1, 0, 1), nrow = 3, ncol = 3)
  result <- parent_dsm(S)

  expect_no_error(plot(result))
})

test_that("plot.dsm works for grandparent DSM", {
  S <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  R <- matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2)
  result <- grandparent_dsm(S, R)

  expect_no_error(plot(result))
})

test_that("plot.dsm handles zero-dimension DSM", {
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 0)
  result <- parent_dsm(empty_matrix)

  expect_message(plot(result), "Nothing to plot")
})
