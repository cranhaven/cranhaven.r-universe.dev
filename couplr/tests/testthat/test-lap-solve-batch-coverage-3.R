# ==============================================================================
# Additional tests for lap_solve_batch.R coverage (parallel execution and edges)
# ==============================================================================

# ------------------------------------------------------------------------------
# Parallel execution tests (with n_threads > 1 and n_problems >= 4)
# ------------------------------------------------------------------------------

test_that("lap_solve_batch parallel execution with matrices", {
  skip_if_not_installed("parallel")

  # Need at least 4 problems to trigger parallel path
  costs <- lapply(1:5, function(i) matrix(runif(9), 3, 3))

  result <- lap_solve_batch(costs, n_threads = 2)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 5)
  expect_true(all(result$total_cost >= 0))
})

test_that("lap_solve_batch with n_threads = NULL uses all cores", {
  skip_if_not_installed("parallel")
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_")),
          "parallel tests limited in check environments")

  costs <- lapply(1:5, function(i) matrix(runif(4), 2, 2))

  # Should not error when n_threads = NULL
  result <- lap_solve_batch(costs, n_threads = NULL)

  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("lap_solve_batch parallel with grouped df", {
  skip_if_not_installed("parallel")

  df <- tibble::tibble(
    sim = rep(1:6, each = 9),
    source = rep(1:3, times = 18),
    target = rep(1:3, each = 3, times = 6),
    cost = runif(54, 1, 10)
  )

  grouped_df <- dplyr::group_by(df, sim)

  result <- lap_solve_batch(grouped_df, source, target, cost, n_threads = 2)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$sim)), 6)
})

# ------------------------------------------------------------------------------
# Error path: grouped df missing all three columns
# ------------------------------------------------------------------------------

test_that("lap_solve_batch_grouped errors on missing all column specs", {
  df <- tibble::tibble(
    grp = rep(1:2, each = 4),
    src = rep(1:2, times = 4),
    tgt = rep(1:2, each = 2, times = 2),
    val = runif(8)
  )

  grouped_df <- dplyr::group_by(df, grp)

  # Missing all three columns
  expect_error(
    lap_solve_batch(grouped_df),
    "must specify"
  )

  # Missing target
  expect_error(
    lap_solve_batch(grouped_df, source = src, cost = val),
    "must specify"
  )

  # Missing cost
  expect_error(
    lap_solve_batch(grouped_df, source = src, target = tgt),
    "must specify"
  )
})

# ------------------------------------------------------------------------------
# Edge cases: empty results (no matched pairs) - triggers branch in map_dfr
# ------------------------------------------------------------------------------

test_that("lap_solve_batch errors on matrices with all forbidden", {
  # Matrix with all Inf (forbidden) - should error
  costs <- list(
    matrix(Inf, 2, 2),
    matrix(c(1, 2, 3, 4), 2, 2)
  )

  expect_error(
    lap_solve_batch(costs),
    "Infeasible|forbidden"
  )
})

# ------------------------------------------------------------------------------
# Print method edge cases
# ------------------------------------------------------------------------------

test_that("print.lap_solve_batch_result handles missing columns gracefully", {
  # Create a result with minimal columns
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))
  result <- lap_solve_batch(costs)

  # Normal print
  output <- capture.output(print(result))
  expect_true(any(grepl("Batch Assignment", output)))

  # Remove problem_id column to test edge case
  result_no_problem <- result
  result_no_problem$problem_id <- NULL
  class(result_no_problem) <- c("lap_solve_batch_result", class(result_no_problem))

  output2 <- capture.output(print(result_no_problem))
  expect_true(any(grepl("Batch Assignment", output2)))
})

test_that("print.lap_solve_batch_result handles empty total_cost", {
  # Result without total_cost column
  result <- tibble::tibble(
    problem_id = 1,
    source = 1L,
    target = 1L,
    cost = 1.0
  )
  class(result) <- c("lap_solve_batch_result", class(result))

  output <- capture.output(print(result))
  expect_true(any(grepl("Batch Assignment", output)))
})

# ------------------------------------------------------------------------------
# Method variations in batch
# ------------------------------------------------------------------------------

test_that("lap_solve_batch works with various methods", {
  costs <- lapply(1:4, function(i) matrix(runif(9), 3, 3))

  for (m in c("auto", "jv", "hungarian")) {
    result <- lap_solve_batch(costs, method = m)
    expect_s3_class(result, "lap_solve_batch_result")
    expect_true(all(result$method_used != ""))
  }
})

# ------------------------------------------------------------------------------
# 3D array dimension variations
# ------------------------------------------------------------------------------

test_that("lap_solve_batch handles 3D array with rectangular slices", {
  # 2x3 slices
  arr <- array(runif(6 * 4), dim = c(2, 3, 4))

  result <- lap_solve_batch(arr)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 4)
})

test_that("lap_solve_batch handles 3D array with single slice", {
  arr <- array(runif(4), dim = c(2, 2, 1))

  result <- lap_solve_batch(arr)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 1)
})

# ------------------------------------------------------------------------------
# Grouped df with different methods
# ------------------------------------------------------------------------------

test_that("lap_solve_batch grouped with different methods", {
  df <- tibble::tibble(
    grp = rep(1:2, each = 4),
    src = rep(1:2, times = 4),
    tgt = rep(1:2, each = 2, times = 2),
    val = runif(8, 1, 10)
  )

  grouped_df <- dplyr::group_by(df, grp)

  for (m in c("auto", "jv", "hungarian")) {
    result <- lap_solve_batch(grouped_df, src, tgt, val, method = m)
    expect_s3_class(result, "lap_solve_batch_result")
  }
})

# ------------------------------------------------------------------------------
# maximize = TRUE in batch
# ------------------------------------------------------------------------------

test_that("lap_solve_batch maximize with grouped df", {
  df <- tibble::tibble(
    grp = rep(1:3, each = 4),
    src = rep(1:2, times = 6),
    tgt = rep(1:2, each = 2, times = 3),
    val = runif(12, 1, 10)
  )

  grouped_df <- dplyr::group_by(df, grp)

  result_min <- lap_solve_batch(grouped_df, src, tgt, val, maximize = FALSE)
  result_max <- lap_solve_batch(grouped_df, src, tgt, val, maximize = TRUE)

  expect_s3_class(result_max, "lap_solve_batch_result")
  # Maximized total cost should be >= minimized
  expect_true(sum(result_max$total_cost) >= sum(result_min$total_cost))
})
