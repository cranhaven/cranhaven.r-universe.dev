# ==============================================================================
# Tests for batch processing (lap_solve_batch)
# ==============================================================================

test_that("lap_solve_batch works with list of matrices", {
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2),
    matrix(c(1, 9, 9, 1), 2, 2)
  )

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_true("problem_id" %in% names(result))
  expect_true("source" %in% names(result))
  expect_true("target" %in% names(result))
  expect_true("cost" %in% names(result))
  expect_true("total_cost" %in% names(result))
  expect_true("method_used" %in% names(result))
  expect_equal(length(unique(result$problem_id)), 3)
})

test_that("lap_solve_batch works with 3D array", {
  arr <- array(runif(2 * 2 * 5), dim = c(2, 2, 5))

  result <- lap_solve_batch(arr)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 5)
})

test_that("lap_solve_batch respects maximize parameter", {
  costs <- list(
    matrix(c(1, 100, 100, 1), 2, 2)
  )

  result_min <- lap_solve_batch(costs, maximize = FALSE)
  result_max <- lap_solve_batch(costs, maximize = TRUE)

  # Minimum should be 2, maximum should be 200
  expect_equal(unique(result_min$total_cost), 2)
  expect_equal(unique(result_max$total_cost), 200)
})

test_that("lap_solve_batch respects method parameter", {
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))

  result <- lap_solve_batch(costs, method = "hungarian")

  expect_equal(unique(result$method_used), "hungarian")
})

test_that("lap_solve_batch handles single problem", {
  costs <- list(matrix(c(4, 2, 3, 1), 2, 2))

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 1)
})

test_that("lap_solve_batch handles rectangular matrices", {
  costs <- list(
    matrix(1:6, 2, 3),
    matrix(1:12, 3, 4)
  )

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 2)
})

test_that("lap_solve_batch handles NA as forbidden", {
  costs <- list(
    matrix(c(1, NA, NA, 1), 2, 2)
  )

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
  # Should match 1->1, 2->2 with total cost 2
  expect_equal(unique(result$total_cost), 2)
})

test_that("lap_solve_batch errors on empty input", {
  expect_error(
    lap_solve_batch(list()),
    "at least one problem"
  )
})

test_that("lap_solve_batch errors on invalid input type", {
  expect_error(
    lap_solve_batch("not a list or array"),
    "must be a list"
  )
})

test_that("lap_solve_batch errors on ungrouped data frame without columns", {
  df <- data.frame(x = 1:5, y = 1:5, z = 1:5)

  expect_error(
    lap_solve_batch(df, source = "x"),
    "use group_by"
  )
})

test_that("lap_solve_batch with n_threads handles sequential fallback", {
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  # With n_threads = 1 (explicit sequential)
  result <- lap_solve_batch(costs, n_threads = 1)
  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("lap_solve_batch with n_threads = NULL uses available cores", {
  skip_on_cran()

  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  # Should not error
  expect_no_error(lap_solve_batch(costs, n_threads = NULL))
})

test_that("lap_solve_batch print method works", {
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  result <- lap_solve_batch(costs)

  expect_output(print(result), "Batch Assignment Results")
  expect_output(print(result), "Number of problems solved")
  expect_output(print(result), "Total cost range")
})

test_that("lap_solve_batch works with grouped data frame", {
  skip_if_not_installed("dplyr")

  df <- tibble::tibble(
    sim = rep(1:3, each = 4),
    source = rep(1:2, times = 6),
    target = rep(1:2, each = 2, times = 3),
    cost = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  )

  grouped_df <- dplyr::group_by(df, sim)

  result <- lap_solve_batch(grouped_df, source, target, cost)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_true("sim" %in% names(result))
  expect_equal(length(unique(result$sim)), 3)
})

test_that("lap_solve_batch grouped requires all columns", {
  skip_if_not_installed("dplyr")

  df <- tibble::tibble(
    sim = rep(1:2, each = 4),
    source = rep(1:2, times = 4),
    target = rep(1:2, each = 2, times = 2),
    cost = 1:8
  )

  grouped_df <- dplyr::group_by(df, sim)

  expect_error(
    lap_solve_batch(grouped_df),
    "must specify"
  )
})

test_that("lap_solve_batch handles 1x1 matrices", {
  costs <- list(
    matrix(5, 1, 1),
    matrix(10, 1, 1)
  )

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(result$total_cost[result$problem_id == 1][1], 5)
  expect_equal(result$total_cost[result$problem_id == 2][1], 10)
})

test_that("lap_solve_batch handles large batch", {
  skip_on_cran()

  costs <- lapply(1:100, function(i) matrix(runif(16), 4, 4))

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 100)
})
