# ==============================================================================
# Additional tests for lap_solve_batch.R coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# Error paths
# ------------------------------------------------------------------------------

test_that("lap_solve_batch errors on non-grouped data frame with source column", {
  skip_on_cran()
  df <- data.frame(src = 1:3, tgt = 2:4, cost = c(1, 2, 3))

  # When given a raw data frame with source column specified as string,
  # should error about needing group_by
  expect_error(
    lap_solve_batch(df, source = "src"),
    "use group_by"
  )
})

test_that("lap_solve_batch errors on invalid x type", {
  skip_on_cran()
  expect_error(
    lap_solve_batch("invalid"),
    "must be a list"
  )
})

test_that("lap_solve_batch errors on empty input", {
  skip_on_cran()
  expect_error(
    lap_solve_batch(list()),
    "at least one problem"
  )
})

test_that("lap_solve_batch_grouped errors on missing columns", {
  skip_on_cran()
  df <- data.frame(
    group = rep(1:2, each = 4),
    src = rep(1:2, 4),
    tgt = rep(1:2, each = 2, times = 2),
    val = runif(8)
  )

  grouped_df <- dplyr::group_by(df, group)

  # Missing source
  expect_error(
    lap_solve_batch(grouped_df, target = tgt, cost = val),
    "must specify"
  )
})

# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("lap_solve_batch handles 3D array", {
  skip_on_cran()
  arr <- array(runif(2 * 2 * 3), dim = c(2, 2, 3))

  result <- lap_solve_batch(arr)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 3)
})

test_that("lap_solve_batch handles maximize", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 10, 10, 1), 2, 2),
    matrix(c(5, 1, 1, 5), 2, 2)
  )

  result_min <- lap_solve_batch(costs, maximize = FALSE)
  result_max <- lap_solve_batch(costs, maximize = TRUE)

  expect_true(all(result_max$total_cost >= result_min$total_cost))
})

test_that("lap_solve_batch handles rectangular matrices", {
  skip_on_cran()
  costs <- list(
    matrix(1:6, 2, 3),
    matrix(1:6, 3, 2)
  )

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("lap_solve_batch handles single problem", {
  skip_on_cran()
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))

  result <- lap_solve_batch(costs)

  expect_equal(length(unique(result$problem_id)), 1)
})

test_that("lap_solve_batch handles n_threads specification", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  # n_threads = 1
  result1 <- lap_solve_batch(costs, n_threads = 1)
  expect_s3_class(result1, "lap_solve_batch_result")

  # n_threads as string should work (coerced)
  result2 <- lap_solve_batch(costs, n_threads = "1")
  expect_s3_class(result2, "lap_solve_batch_result")
})

test_that("lap_solve_batch handles different methods", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  costs <- list(cost, cost)

  for (method in c("jv", "hungarian", "auction")) {
    result <- lap_solve_batch(costs, method = method)
    expect_s3_class(result, "lap_solve_batch_result")
  }
})

# ------------------------------------------------------------------------------
# Grouped data frame tests
# ------------------------------------------------------------------------------

test_that("lap_solve_batch handles grouped data frame", {
  skip_on_cran()
  df <- tibble::tibble(
    sim = rep(1:3, each = 4),
    source = rep(1:2, times = 6),
    target = rep(1:2, each = 2, times = 3),
    cost = runif(12, 1, 10)
  )

  grouped_df <- dplyr::group_by(df, sim)

  result <- lap_solve_batch(grouped_df, source, target, cost)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_true("sim" %in% names(result))
})

test_that("lap_solve_batch handles grouped df with maximize", {
  skip_on_cran()
  df <- tibble::tibble(
    grp = rep(1:2, each = 4),
    src = rep(1:2, times = 4),
    tgt = rep(1:2, each = 2, times = 2),
    val = c(1, 5, 5, 1, 2, 6, 6, 2)
  )

  grouped_df <- dplyr::group_by(df, grp)

  result <- lap_solve_batch(grouped_df, src, tgt, val, maximize = TRUE)

  expect_s3_class(result, "lap_solve_batch_result")
})

# ------------------------------------------------------------------------------
# Print method tests
# ------------------------------------------------------------------------------

test_that("print.lap_solve_batch_result works", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  result <- lap_solve_batch(costs)

  output <- capture.output(print(result))

  expect_true(any(grepl("Batch Assignment", output)))
  expect_true(any(grepl("Number of problems", output)))
})

test_that("print.lap_solve_batch_result handles edge cases", {
  skip_on_cran()
  costs <- list(matrix(1, 1, 1))
  result <- lap_solve_batch(costs)

  output <- capture.output(print(result))
  expect_true(any(grepl("Batch Assignment", output)))
})
