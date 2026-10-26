# ==============================================================================
# Final coverage tests for lap_solve_batch.R
# ==============================================================================

test_that("lap_solve_batch errors on data frame without group_by", {
  skip_on_cran()
  df <- tibble::tibble(source = 1:3, target = 1:3, cost = c(1, 2, 3))
  expect_error(
    couplr::lap_solve_batch(df, source = source, target = target, cost = cost),
    "group_by"
  )
})

test_that("lap_solve_batch errors on invalid input type", {
  skip_on_cran()
  expect_error(
    couplr::lap_solve_batch("invalid"),
    "list of matrices"
  )
})

test_that("lap_solve_batch errors on empty input", {
  skip_on_cran()
  expect_error(
    couplr::lap_solve_batch(list()),
    "at least one problem"
  )
})

test_that("lap_solve_batch handles 3D array", {
  skip_on_cran()
  arr <- array(runif(2 * 2 * 3), dim = c(2, 2, 3))
  result <- couplr::lap_solve_batch(arr)
  expect_s3_class(result, "lap_solve_batch_result")
  expect_true(dplyr::n_distinct(result$problem_id) == 3)
})

test_that("lap_solve_batch handles n_threads = NULL", {
  skip_on_cran()
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))
  # Should use detectCores()
  result <- couplr::lap_solve_batch(costs, n_threads = NULL)
  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("lap_solve_batch with grouped data frame", {
  skip_on_cran()
  df <- tibble::tibble(
    sim = rep(1:2, each = 4),
    source = rep(1:2, 4),
    target = rep(c(1, 1, 2, 2), 2),
    cost = c(1, 5, 5, 1, 2, 4, 4, 2)
  )

  result <- df |>
    dplyr::group_by(sim) |>
    couplr::lap_solve_batch(source = source, target = target, cost = cost)

  expect_s3_class(result, "lap_solve_batch_result")
})

test_that("lap_solve_batch_grouped errors without column specs", {
  skip_on_cran()
  df <- tibble::tibble(
    sim = rep(1:2, each = 4),
    source = rep(1:2, 4),
    target = rep(c(1, 1, 2, 2), 2),
    cost = c(1, 5, 5, 1, 2, 4, 4, 2)
  )

  expect_error(
    df |> dplyr::group_by(sim) |> couplr::lap_solve_batch(),
    "source.*target.*cost"
  )
})

test_that("print.lap_solve_batch_result works", {
  skip_on_cran()
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2), matrix(c(5, 6, 7, 8), 2, 2))
  result <- couplr::lap_solve_batch(costs)

  expect_output(print(result), "Batch Assignment Results")
  expect_output(print(result), "Number of problems")
  expect_output(print(result), "Total cost range")
})

test_that("print.lap_solve_batch_result handles missing columns", {
  skip_on_cran()
  # Create result without problem_id
  result <- tibble::tibble(source = 1:2, target = 2:1, cost = c(1, 2))
  class(result) <- c("lap_solve_batch_result", class(result))

  expect_output(print(result), "Batch Assignment Results")
})

test_that("lap_solve_batch handles maximize = TRUE", {
  skip_on_cran()
  costs <- list(matrix(c(1, 10, 10, 1), 2, 2))
  result <- couplr::lap_solve_batch(costs, maximize = TRUE)
  expect_s3_class(result, "lap_solve_batch_result")
})
