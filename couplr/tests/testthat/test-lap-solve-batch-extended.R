# ==============================================================================
# Extended tests for lap_solve_batch.R
# ==============================================================================

# ------------------------------------------------------------------------------
# Basic functionality
# ------------------------------------------------------------------------------

test_that("lap_solve_batch works with list of matrices", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_true("problem_id" %in% names(result))
  expect_equal(length(unique(result$problem_id)), 2)
})

test_that("lap_solve_batch works with 3D array", {
  skip_on_cran()
  arr <- array(runif(2 * 2 * 5), dim = c(2, 2, 5))

  result <- lap_solve_batch(arr)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 5)
})

test_that("lap_solve_batch with maximize = TRUE", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  result_min <- lap_solve_batch(costs, maximize = FALSE)
  result_max <- lap_solve_batch(costs, maximize = TRUE)

  # Maximized total cost should generally be >= minimized
  min_costs <- unique(result_min[, c("problem_id", "total_cost")])$total_cost
  max_costs <- unique(result_max[, c("problem_id", "total_cost")])$total_cost

  expect_true(all(max_costs >= min_costs))
})

test_that("lap_solve_batch with specific method", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  result <- lap_solve_batch(costs, method = "hungarian")

  expect_true(all(result$method_used == "hungarian"))
})

# ------------------------------------------------------------------------------
# Error cases
# ------------------------------------------------------------------------------

test_that("lap_solve_batch errors on empty list", {
  skip_on_cran()
  expect_error(
    lap_solve_batch(list()),
    "at least one problem"
  )
})

test_that("lap_solve_batch errors on invalid input type", {
  skip_on_cran()
  expect_error(
    lap_solve_batch("invalid"),
    "must be a list of matrices, 3D array, or grouped data frame"
  )
})

test_that("lap_solve_batch errors on data frame without grouping", {
  skip_on_cran()
  df <- data.frame(source = 1:4, target = 1:4, cost = runif(4))

  expect_error(
    lap_solve_batch(df, source = source, target = target, cost = cost),
    "use group_by"
  )
})

# ------------------------------------------------------------------------------
# Grouped data frame input
# ------------------------------------------------------------------------------

test_that("lap_solve_batch works with grouped data frame", {
  skip_on_cran()
  skip_if_not_installed("dplyr")

  df <- tibble::tibble(
    sim = rep(1:3, each = 4),
    source = rep(1:2, times = 6),
    target = rep(1:2, each = 2, times = 3),
    cost = runif(12, 1, 10)
  )

  result <- df |>
    dplyr::group_by(sim) |>
    lap_solve_batch(source = source, target = target, cost = cost)

  expect_s3_class(result, "lap_solve_batch_result")
  expect_true("sim" %in% names(result))
})

test_that("lap_solve_batch_grouped errors without column specs", {
  skip_on_cran()
  skip_if_not_installed("dplyr")

  df <- tibble::tibble(
    sim = rep(1:2, each = 4),
    source = rep(1:2, times = 4),
    target = rep(1:2, each = 2, times = 2),
    cost = runif(8)
  )

  expect_error(
    df |> dplyr::group_by(sim) |> lap_solve_batch(),
    "must specify.*source.*target.*cost"
  )
})

test_that("lap_solve_batch with n_threads = NULL uses all cores", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  # Should not error
  result <- lap_solve_batch(costs, n_threads = NULL)

  expect_s3_class(result, "lap_solve_batch_result")
})

# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("lap_solve_batch handles single matrix", {
  skip_on_cran()
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))

  result <- lap_solve_batch(costs)

  expect_equal(length(unique(result$problem_id)), 1)
})

test_that("lap_solve_batch handles rectangular matrices", {
  skip_on_cran()
  costs <- list(
    matrix(1:6, 2, 3),
    matrix(1:6, 2, 3)
  )

  result <- lap_solve_batch(costs)

  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("lap_solve_batch handles 1x1 matrices", {
  skip_on_cran()
  costs <- list(
    matrix(5, 1, 1),
    matrix(10, 1, 1)
  )

  result <- lap_solve_batch(costs)

  expect_equal(unique(result$source), 1L)
  expect_equal(unique(result$target), 1L)
})

# ------------------------------------------------------------------------------
# Print method
# ------------------------------------------------------------------------------

test_that("print.lap_solve_batch_result works", {
  skip_on_cran()
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )

  result <- lap_solve_batch(costs)

  output <- capture.output(print(result))

  expect_true(any(grepl("Batch Assignment Results", output)))
  expect_true(any(grepl("problems solved", output)))
})

test_that("print.lap_solve_batch_result handles missing columns gracefully", {
  skip_on_cran()
  # Create a tibble that looks like lap_solve_batch result but missing columns
  result <- tibble::tibble(
    source = 1:2,
    target = 2:1,
    cost = c(3, 3)
  )
  class(result) <- c("lap_solve_batch_result", class(result))

  # Should still print without error
  output <- capture.output(print(result))

  expect_true(any(grepl("Batch Assignment Results", output)))
})

# ------------------------------------------------------------------------------
# Consistency with single lap_solve
# ------------------------------------------------------------------------------

test_that("lap_solve_batch matches individual lap_solve results", {
  skip_on_cran()
  set.seed(123)
  cost1 <- matrix(runif(16), 4, 4)
  cost2 <- matrix(runif(16), 4, 4)

  # Batch solve
  batch_result <- lap_solve_batch(list(cost1, cost2))

  # Individual solves
  single1 <- assignment(cost1)
  single2 <- assignment(cost2)

  # Compare total costs
  batch_costs <- unique(batch_result[, c("problem_id", "total_cost")])

  expect_equal(batch_costs$total_cost[1], single1$total_cost, tolerance = 1e-9)
  expect_equal(batch_costs$total_cost[2], single2$total_cost, tolerance = 1e-9)
})
