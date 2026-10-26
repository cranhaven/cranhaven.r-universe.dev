# ==============================================================================
# Extended coverage tests for lap_solve_batch.R and lap_solve_kbest.R
# ==============================================================================

# ------------------------------------------------------------------------------
# lap_solve_batch edge cases
# ------------------------------------------------------------------------------

test_that("lap_solve_batch errors on data frame without group_by", {
  skip_on_cran()
  df <- data.frame(source = 1:4, target = 1:4, cost = 1:4)
  expect_error(lap_solve_batch(df, source = source, target = target, cost = cost),
               "group_by")
})

test_that("lap_solve_batch errors on invalid input type", {
  skip_on_cran()
  expect_error(lap_solve_batch("invalid"), "must be a list")
})

test_that("lap_solve_batch errors on empty list", {
  skip_on_cran()
  expect_error(lap_solve_batch(list()), "at least one problem")
})

test_that("lap_solve_batch handles 3D array input", {
  skip_on_cran()
  arr <- array(runif(2 * 2 * 5), dim = c(2, 2, 5))
  result <- lap_solve_batch(arr)
  expect_s3_class(result, "lap_solve_batch_result")
  expect_equal(length(unique(result$problem_id)), 5)
})

test_that("lap_solve_batch handles n_threads = NULL", {
  skip_on_cran()
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))
  # n_threads = NULL should auto-detect cores but not fail
  result <- lap_solve_batch(costs, n_threads = NULL)
  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("lap_solve_batch with grouped data frame", {
  skip_on_cran()
  df <- tibble::tibble(
    sim = rep(1:3, each = 4),
    source = rep(1:2, times = 6),
    target = rep(1:2, each = 2, times = 3),
    cost = runif(12, 1, 10)
  )
  grouped_df <- dplyr::group_by(df, sim)
  result <- lap_solve_batch(grouped_df, source = source, target = target, cost = cost)
  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("print.lap_solve_batch_result works", {
  skip_on_cran()
  costs <- list(matrix(c(1, 5, 5, 1), 2, 2), matrix(c(2, 4, 4, 2), 2, 2))
  result <- lap_solve_batch(costs)
  expect_output(print(result), "Batch Assignment Results")
  expect_output(print(result), "Number of problems solved:")
})

test_that("print.lap_solve_batch_result handles missing columns", {
  skip_on_cran()
  # Create result with minimal columns
  result <- tibble::tibble(source = 1:2, target = 2:1)
  class(result) <- c("lap_solve_batch_result", class(result))
  expect_output(print(result), "Batch Assignment Results")
})

# ------------------------------------------------------------------------------
# lap_solve_kbest edge cases
# ------------------------------------------------------------------------------

test_that("lap_solve_kbest errors on data frame without required columns", {
  skip_on_cran()
  df <- data.frame(x = 1:4, y = 1:4)
  expect_error(lap_solve_kbest(df, k = 2), "must specify")
})

test_that("lap_solve_kbest with data frame input", {
  skip_on_cran()
  df <- tibble::tibble(
    source = rep(1:3, each = 3),
    target = rep(1:3, times = 3),
    cost = c(4, 2, 5, 3, 3, 6, 7, 5, 4)
  )
  result <- lap_solve_kbest(df, k = 3, source = source, target = target, cost = cost)
  expect_s3_class(result, "lap_solve_kbest_result")
  expect_true(length(unique(result$solution_id)) <= 3)
})

test_that("lap_solve_kbest with maximization", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
  result <- lap_solve_kbest(cost, k = 3, maximize = TRUE)
  expect_s3_class(result, "lap_solve_kbest_result")
})

test_that("print.lap_solve_kbest_result with many solutions", {
  skip_on_cran()
  cost <- matrix(runif(36), 6, 6)
  result <- lap_solve_kbest(cost, k = 10)
  expect_output(print(result), "K-Best Assignment Results")
  if (length(unique(result$solution_id)) > 5) {
    expect_output(print(result), "more solutions")
  }
})

test_that("summary.lap_solve_kbest_result works", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
  result <- lap_solve_kbest(cost, k = 3)
  summ <- summary(result)
  expect_true(tibble::is_tibble(summ))
  expect_true("n_assignments" %in% names(summ))
})

test_that("summary.lap_solve_kbest_result handles empty result", {
  skip_on_cran()
  # Create empty result
  result <- tibble::tibble(
    rank = integer(0),
    solution_id = integer(0),
    source = integer(0),
    target = integer(0),
    cost = numeric(0),
    total_cost = numeric(0)
  )
  class(result) <- c("lap_solve_kbest_result", class(result))
  summ <- summary(result)
  expect_equal(nrow(summ), 0)
})

test_that("print.lap_solve_kbest_result handles empty result", {
  skip_on_cran()
  result <- tibble::tibble(
    rank = integer(0),
    solution_id = integer(0),
    source = integer(0),
    target = integer(0),
    cost = numeric(0),
    total_cost = numeric(0)
  )
  class(result) <- c("lap_solve_kbest_result", class(result))
  expect_output(print(result), "No solutions found")
})

# ------------------------------------------------------------------------------
# kbest_assignment internal function
# ------------------------------------------------------------------------------

test_that("kbest_assignment errors on non-numeric cost", {
  skip_on_cran()
  cost <- matrix(letters[1:4], 2, 2)
  expect_error(kbest_assignment(cost), "numeric")
})

test_that("kbest_assignment errors on NaN", {
  skip_on_cran()
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)
  expect_error(kbest_assignment(cost), "NaN")
})

# ------------------------------------------------------------------------------
# lap_solve_batch_grouped edge cases
# ------------------------------------------------------------------------------

test_that("lap_solve_batch_grouped errors without required columns", {
  skip_on_cran()
  df <- tibble::tibble(
    sim = rep(1:2, each = 4),
    x = 1:8,
    y = 8:1
  )
  grouped_df <- dplyr::group_by(df, sim)
  expect_error(lap_solve_batch(grouped_df), "must specify")
})
