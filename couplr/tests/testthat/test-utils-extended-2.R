# ==============================================================================
# Extended coverage tests for utils.R
# ==============================================================================

# ------------------------------------------------------------------------------
# validate_cost_data
# ------------------------------------------------------------------------------

test_that("validate_cost_data errors on data frame", {
  skip_on_cran()
  df <- data.frame(x = 1:3, y = 1:3)
  expect_error(couplr:::validate_cost_data(df), "Data frame")
})

test_that("validate_cost_data errors on empty matrix", {
  skip_on_cran()
  m <- matrix(nrow = 0, ncol = 0)
  expect_error(couplr:::validate_cost_data(m), "at least one row")
})

test_that("validate_cost_data errors on non-numeric", {
  skip_on_cran()
  m <- matrix(letters[1:4], 2, 2)
  expect_error(couplr:::validate_cost_data(m), "numeric")
})

test_that("validate_cost_data errors on NaN", {
  skip_on_cran()
  m <- matrix(c(1, NaN, 3, 4), 2, 2)
  expect_error(couplr:::validate_cost_data(m), "NaN")
})

test_that("validate_cost_data returns matrix", {
  skip_on_cran()
  m <- matrix(1:4, 2, 2)
  result <- couplr:::validate_cost_data(m)
  expect_true(is.matrix(result))
})

# ------------------------------------------------------------------------------
# Type checking functions
# ------------------------------------------------------------------------------

test_that("is_lap_solve_result works", {
  skip_on_cran()
  expect_false(is_lap_solve_result(data.frame()))

  # Create proper result
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)
  expect_true(is_lap_solve_result(result))
})

test_that("is_lap_solve_batch_result works", {
  skip_on_cran()
  expect_false(is_lap_solve_batch_result(data.frame()))

  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))
  result <- lap_solve_batch(costs)
  expect_true(is_lap_solve_batch_result(result))
})

test_that("is_lap_solve_kbest_result works", {
  skip_on_cran()
  expect_false(is_lap_solve_kbest_result(data.frame()))

  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve_kbest(cost, k = 2)
  expect_true(is_lap_solve_kbest_result(result))
})

# ------------------------------------------------------------------------------
# get_total_cost
# ------------------------------------------------------------------------------

test_that("get_total_cost works for lap_solve_result", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)
  tc <- get_total_cost(result)
  expect_true(is.numeric(tc))
})

test_that("get_total_cost works for batch result", {
  skip_on_cran()
  costs <- list(matrix(c(1, 5, 5, 1), 2, 2), matrix(c(2, 4, 4, 2), 2, 2))
  result <- lap_solve_batch(costs)
  tc <- get_total_cost(result)
  expect_true(is.numeric(tc))
  expect_length(tc, 2)
})

test_that("get_total_cost works for kbest result", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve_kbest(cost, k = 2)
  tc <- get_total_cost(result)
  expect_true(is.numeric(tc))
})

test_that("get_total_cost errors on invalid object", {
  skip_on_cran()
  expect_error(get_total_cost(data.frame()), "not a valid")
})

test_that("get_total_cost errors when attr missing", {
  skip_on_cran()
  result <- tibble::tibble(source = 1, target = 1, cost = 1)
  class(result) <- c("lap_solve_result", class(result))
  expect_error(get_total_cost(result), "not found")
})

# ------------------------------------------------------------------------------
# get_method_used
# ------------------------------------------------------------------------------

test_that("get_method_used works for lap_solve_result", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)
  m <- get_method_used(result)
  expect_true(is.character(m))
})

test_that("get_method_used works for batch result", {
  skip_on_cran()
  costs <- list(matrix(c(1, 5, 5, 1), 2, 2))
  result <- lap_solve_batch(costs)
  m <- get_method_used(result)
  expect_true(is.character(m))
})

test_that("get_method_used errors on invalid object", {
  skip_on_cran()
  expect_error(get_method_used(data.frame()), "not a valid")
})

test_that("get_method_used errors when attr missing", {
  skip_on_cran()
  result <- tibble::tibble(source = 1, target = 1, cost = 1)
  class(result) <- c("lap_solve_result", class(result))
  expect_error(get_method_used(result), "not found")
})

# ------------------------------------------------------------------------------
# as_assignment_matrix
# ------------------------------------------------------------------------------

test_that("as_assignment_matrix creates binary matrix", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)
  mat <- as_assignment_matrix(result)
  expect_true(is.matrix(mat))
  expect_true(all(mat %in% c(0L, 1L)))
})

test_that("as_assignment_matrix with explicit dimensions", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)
  mat <- as_assignment_matrix(result, n_sources = 3, n_targets = 3)
  expect_equal(dim(mat), c(3, 3))
})

test_that("as_assignment_matrix handles empty result", {
  skip_on_cran()
  result <- tibble::tibble(
    source = integer(0),
    target = integer(0),
    cost = numeric(0)
  )
  class(result) <- c("lap_solve_result", class(result))
  attr(result, "total_cost") <- 0

  mat <- as_assignment_matrix(result, n_sources = 2, n_targets = 2)
  expect_equal(dim(mat), c(2, 2))
  expect_true(all(mat == 0L))
})

test_that("as_assignment_matrix errors on non-result", {
  skip_on_cran()
  expect_error(as_assignment_matrix(data.frame()), "must be a lap_solve_result")
})

test_that("as_assignment_matrix errors on missing columns", {
  skip_on_cran()
  result <- tibble::tibble(x = 1, y = 2)
  class(result) <- c("lap_solve_result", class(result))
  expect_error(as_assignment_matrix(result), "source.*target")
})

test_that("as_assignment_matrix errors on negative dimensions", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)
  expect_error(as_assignment_matrix(result, n_sources = -1), "non-negative")
})
