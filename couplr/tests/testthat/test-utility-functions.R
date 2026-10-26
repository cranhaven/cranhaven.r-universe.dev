# ==============================================================================
# Tests for utility functions (utils.R)
# ==============================================================================

# ------------------------------------------------------------------------------
# validate_cost_data tests
# ------------------------------------------------------------------------------

test_that("validate_cost_data accepts valid numeric matrix", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- couplr:::validate_cost_data(cost)

  expect_equal(result, cost)
})

test_that("validate_cost_data converts numeric vector to matrix", {
  cost <- c(1, 2, 3, 4)

  result <- couplr:::validate_cost_data(cost)

  expect_true(is.matrix(result))
})

test_that("validate_cost_data errors on data frame", {
  cost <- data.frame(x = 1:3, y = 1:3, z = 1:3)

  expect_error(
    couplr:::validate_cost_data(cost),
    "Data frame input"
  )
})

test_that("validate_cost_data errors on empty matrix", {
  cost <- matrix(nrow = 0, ncol = 0)

  expect_error(
    couplr:::validate_cost_data(cost),
    "at least one row"
  )
})

test_that("validate_cost_data errors on non-numeric matrix", {
  cost <- matrix(c("a", "b", "c", "d"), 2, 2)

  expect_error(
    couplr:::validate_cost_data(cost),
    "must be numeric"
  )
})

test_that("validate_cost_data errors on NaN values", {
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)

  expect_error(
    couplr:::validate_cost_data(cost),
    "NaN"
  )
})

test_that("validate_cost_data allows NA values", {
  cost <- matrix(c(1, NA, 3, 4), 2, 2)  # NA is at [2,1] in column-major order

  result <- couplr:::validate_cost_data(cost)

  expect_true(is.na(result[2, 1]))
})

test_that("validate_cost_data allows Inf values", {
  cost <- matrix(c(1, Inf, 3, 4), 2, 2)  # Inf is at [2,1] in column-major order

  result <- couplr:::validate_cost_data(cost)

  expect_true(is.infinite(result[2, 1]))
})

# ------------------------------------------------------------------------------
# is_* predicate function tests
# ------------------------------------------------------------------------------

test_that("is_lap_solve_result returns TRUE for lap_solve result", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost)

  expect_true(is_lap_solve_result(result))
})

test_that("is_lap_solve_result returns FALSE for other objects", {
  expect_false(is_lap_solve_result(list()))
  expect_false(is_lap_solve_result(data.frame()))
  expect_false(is_lap_solve_result(NULL))
  expect_false(is_lap_solve_result(1:5))
})

test_that("is_lap_solve_batch_result returns TRUE for batch result", {
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))
  result <- lap_solve_batch(costs)

  expect_true(is_lap_solve_batch_result(result))
})

test_that("is_lap_solve_batch_result returns FALSE for other objects", {
  expect_false(is_lap_solve_batch_result(list()))
  expect_false(is_lap_solve_batch_result(data.frame()))
  expect_false(is_lap_solve_batch_result(NULL))
})

test_that("is_lap_solve_kbest_result returns TRUE for kbest result", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve_kbest(cost, k = 2)

  expect_true(is_lap_solve_kbest_result(result))
})

test_that("is_lap_solve_kbest_result returns FALSE for other objects", {
  expect_false(is_lap_solve_kbest_result(list()))
  expect_false(is_lap_solve_kbest_result(data.frame()))
})

# ------------------------------------------------------------------------------
# get_total_cost tests
# ------------------------------------------------------------------------------

test_that("get_total_cost works for lap_solve_result", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost)

  tc <- get_total_cost(result)

  expect_type(tc, "double")
  expect_true(tc > 0)
})

test_that("get_total_cost works for lap_solve_batch_result", {
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )
  result <- lap_solve_batch(costs)

  tc <- get_total_cost(result)

  expect_type(tc, "double")
  expect_equal(length(tc), 2)  # Two problems
})

test_that("get_total_cost works for lap_solve_kbest_result", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve_kbest(cost, k = 2)

  tc <- get_total_cost(result)

  expect_type(tc, "double")
  expect_true(length(tc) >= 1)
})

test_that("get_total_cost errors on invalid object", {
  expect_error(
    get_total_cost(list()),
    "not a valid assignment result"
  )
})

test_that("get_total_cost errors on lap_solve_result without attribute", {
  result <- tibble::tibble(source = 1:2, target = 2:1, cost = c(1, 2))
  class(result) <- c("lap_solve_result", class(result))
  # No total_cost attribute

  expect_error(
    get_total_cost(result),
    "total_cost attribute not found"
  )
})

# ------------------------------------------------------------------------------
# get_method_used tests
# ------------------------------------------------------------------------------

test_that("get_method_used works for lap_solve_result", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost, method = "hungarian")

  method <- get_method_used(result)

  expect_type(method, "character")
  expect_equal(method, "hungarian")
})

test_that("get_method_used works for lap_solve_batch_result", {
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))
  result <- lap_solve_batch(costs, method = "hungarian")

  method <- get_method_used(result)

  expect_type(method, "character")
  expect_equal(method, "hungarian")
})

test_that("get_method_used errors on invalid object", {
  expect_error(
    get_method_used(list()),
    "not a valid assignment result"
  )
})

test_that("get_method_used errors on lap_solve_result without attribute", {
  result <- tibble::tibble(source = 1:2, target = 2:1, cost = c(1, 2))
  class(result) <- c("lap_solve_result", class(result))
  # No method_used attribute

  expect_error(
    get_method_used(result),
    "method_used attribute not found"
  )
})

# ------------------------------------------------------------------------------
# as_assignment_matrix tests
# ------------------------------------------------------------------------------

test_that("as_assignment_matrix creates binary matrix", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)

  mat <- as_assignment_matrix(result)

  expect_true(is.matrix(mat))
  expect_true(all(mat %in% c(0L, 1L)))
  expect_equal(sum(mat), 2)  # Two assignments
})

test_that("as_assignment_matrix handles custom dimensions", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)

  mat <- as_assignment_matrix(result, n_sources = 5, n_targets = 5)

  expect_equal(nrow(mat), 5)
  expect_equal(ncol(mat), 5)
  expect_equal(sum(mat), 2)
})

test_that("as_assignment_matrix errors on non-lap_solve_result", {
  expect_error(
    as_assignment_matrix(list()),
    "must be a lap_solve_result"
  )
})

test_that("as_assignment_matrix handles empty result", {
  result <- tibble::tibble(
    source = integer(0),
    target = integer(0),
    cost = numeric(0)
  )
  class(result) <- c("lap_solve_result", class(result))
  attr(result, "total_cost") <- 0
  attr(result, "method_used") <- "test"

  mat <- as_assignment_matrix(result, n_sources = 3, n_targets = 3)

  expect_equal(dim(mat), c(3, 3))
  expect_equal(sum(mat), 0)
})

test_that("as_assignment_matrix errors on missing columns", {
  result <- tibble::tibble(x = 1:2, y = 1:2)
  class(result) <- c("lap_solve_result", class(result))
  attr(result, "total_cost") <- 0
  attr(result, "method_used") <- "test"

  expect_error(
    as_assignment_matrix(result),
    "source.*target"
  )
})

test_that("as_assignment_matrix errors on negative dimensions", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)

  expect_error(
    as_assignment_matrix(result, n_sources = -1),
    "non-negative"
  )
})

# ------------------------------------------------------------------------------
# %||% operator tests
# ------------------------------------------------------------------------------

test_that("null coalescing operator returns first if not NULL", {
  result <- couplr:::`%||%`(5, 10)
  expect_equal(result, 5)
})

test_that("null coalescing operator returns second if first is NULL", {
  result <- couplr:::`%||%`(NULL, 10)
  expect_equal(result, 10)
})

test_that("null coalescing operator works with NA", {
  # NA is not NULL, so should return NA
  result <- couplr:::`%||%`(NA, 10)
  expect_true(is.na(result))
})

# ------------------------------------------------------------------------------
# is_distance_object tests
# ------------------------------------------------------------------------------

test_that("is_distance_object returns TRUE for distance_object", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  dist_obj <- compute_distances(left, right, vars = "x")

  expect_true(is_distance_object(dist_obj))
})

test_that("is_distance_object returns FALSE for other objects", {
  expect_false(is_distance_object(list()))
  expect_false(is_distance_object(data.frame()))
  expect_false(is_distance_object(NULL))
  expect_false(is_distance_object(matrix(1:4, 2, 2)))
})
