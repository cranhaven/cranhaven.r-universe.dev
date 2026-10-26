# ==============================================================================
# Coverage tests for matching_constraints.R
# ==============================================================================

test_that("apply_max_distance returns unchanged for NULL", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  result <- couplr:::apply_max_distance(cost, max_distance = NULL)
  expect_equal(result, cost)
})

test_that("apply_max_distance returns unchanged for Inf", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  result <- couplr:::apply_max_distance(cost, max_distance = Inf)
  expect_equal(result, cost)
})

test_that("apply_max_distance errors on non-numeric", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  expect_error(
    couplr:::apply_max_distance(cost, max_distance = "not_numeric"),
    "single numeric"
  )
})

test_that("apply_max_distance errors on vector", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  expect_error(
    couplr:::apply_max_distance(cost, max_distance = c(1, 2)),
    "single numeric"
  )
})

test_that("apply_max_distance errors on non-positive", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  expect_error(
    couplr:::apply_max_distance(cost, max_distance = 0),
    "positive"
  )
  expect_error(
    couplr:::apply_max_distance(cost, max_distance = -1),
    "positive"
  )
})

test_that("apply_max_distance marks forbidden pairs", {
  skip_on_cran()
  # matrix(c(1, 5, 3, 2), 2, 2) fills by column:
  #      [,1] [,2]
  # [1,]    1    3
  # [2,]    5    2
  cost <- matrix(c(1, 5, 3, 2), 2, 2)
  result <- couplr:::apply_max_distance(cost, max_distance = 4)
  # Value 5 at [2,1] exceeds max_distance=4, should be marked BIG_COST
  expect_true(result[2, 1] > 1e10)
  expect_equal(result[1, 1], 1)
  expect_equal(result[1, 2], 3)
  expect_equal(result[2, 2], 2)
})

test_that("apply_calipers returns unchanged for NULL", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  left <- data.frame(x = c(1, 2))
  right <- data.frame(x = c(3, 4))
  result <- couplr:::apply_calipers(cost, left, right, calipers = NULL, vars = "x")
  expect_equal(result, cost)
})

test_that("apply_calipers skips variables not in vars", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  left <- data.frame(x = c(1, 2), y = c(10, 20))
  right <- data.frame(x = c(3, 4), y = c(100, 200))
  # Caliper on y, but y not in vars - should be skipped
  result <- couplr:::apply_calipers(cost, left, right, calipers = list(y = 0.1), vars = "x")
  expect_equal(result, cost)
})

test_that("apply_calipers marks forbidden pairs", {
  skip_on_cran()
  cost <- matrix(1, 2, 2)
  left <- data.frame(x = c(1, 10))
  right <- data.frame(x = c(2, 5))
  # Caliper of 3 on x
  result <- couplr:::apply_calipers(cost, left, right, calipers = list(x = 3), vars = "x")
  # |1-2|=1 OK, |1-5|=4 > 3 forbidden
  # |10-2|=8 > 3 forbidden, |10-5|=5 > 3 forbidden
  expect_equal(result[1, 1], 1)  # OK
  expect_true(result[1, 2] > 1e10)  # Forbidden
  expect_true(result[2, 1] > 1e10)  # Forbidden
  expect_true(result[2, 2] > 1e10)  # Forbidden
})

test_that("mark_forbidden_pairs returns unchanged for NULL", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  result <- couplr:::mark_forbidden_pairs(cost, forbidden_indices = NULL)
  expect_equal(result, cost)
})

test_that("mark_forbidden_pairs returns unchanged for empty matrix", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  result <- couplr:::mark_forbidden_pairs(cost, forbidden_indices = matrix(nrow = 0, ncol = 2))
  expect_equal(result, cost)
})

test_that("mark_forbidden_pairs marks specific pairs", {
  skip_on_cran()
  cost <- matrix(1:4, 2, 2)
  # Forbid pair (1, 2) and (2, 1)
  forbidden <- matrix(c(1, 2, 2, 1), ncol = 2, byrow = TRUE)
  result <- couplr:::mark_forbidden_pairs(cost, forbidden)
  expect_true(result[1, 2] > 1e10)
  expect_true(result[2, 1] > 1e10)
  expect_equal(result[1, 1], 1)
  expect_equal(result[2, 2], 4)
})

test_that("apply_all_constraints combines all constraints", {
  skip_on_cran()
  # matrix(c(1, 5, 3, 2), 2, 2) fills by column:
  #      [,1] [,2]
  # [1,]    1    3
  # [2,]    5    2
  cost <- matrix(c(1, 5, 3, 2), 2, 2)
  left <- data.frame(x = c(1, 10))
  right <- data.frame(x = c(2, 3))

  # max_distance = 4 forbids the 5 at [2,1]
  # caliper on x = 3 forbids pairs where diff > 3
  result <- couplr:::apply_all_constraints(
    cost, left, right, vars = "x",
    max_distance = 4, calipers = list(x = 3)
  )

  # Check that constraints were applied
  expect_true(result[2, 1] > 1e10)  # Was 5, exceeds max_distance AND |10-2|=8 > 3
  expect_true(result[2, 2] > 1e10)  # |10-3|=7 > 3
})

test_that("apply_all_constraints with forbidden pairs", {
  skip_on_cran()
  cost <- matrix(1, 2, 2)
  left <- data.frame(x = c(1, 2))
  right <- data.frame(x = c(1, 2))

  forbidden <- matrix(c(1, 1), ncol = 2)
  result <- couplr:::apply_all_constraints(
    cost, left, right, vars = "x",
    forbidden = forbidden
  )

  expect_true(result[1, 1] > 1e10)
  expect_equal(result[1, 2], 1)
  expect_equal(result[2, 1], 1)
  expect_equal(result[2, 2], 1)
})

test_that("has_valid_pairs returns TRUE when valid pairs exist", {
  skip_on_cran()
  cost <- matrix(c(1, couplr:::BIG_COST, 3, 4), 2, 2)
  expect_true(couplr:::has_valid_pairs(cost))
})

test_that("has_valid_pairs returns FALSE when no valid pairs", {
  skip_on_cran()
  cost <- matrix(couplr:::BIG_COST, 2, 2)
  expect_false(couplr:::has_valid_pairs(cost))
})

test_that("has_valid_pairs handles Inf", {
  skip_on_cran()
  cost <- matrix(Inf, 2, 2)
  expect_false(couplr:::has_valid_pairs(cost))
})

test_that("count_valid_pairs counts correctly", {
  skip_on_cran()
  cost <- matrix(c(1, couplr:::BIG_COST, Inf, 4), 2, 2)
  expect_equal(couplr:::count_valid_pairs(cost), 2)  # Only 1 and 4 are valid
})

test_that("count_valid_pairs returns 0 for all forbidden", {
  skip_on_cran()
  cost <- matrix(couplr:::BIG_COST, 3, 3)
  expect_equal(couplr:::count_valid_pairs(cost), 0)
})

test_that("BIG_COST is accessible", {
  skip_on_cran()
  expect_true(couplr:::BIG_COST > 1e10)
  expect_true(is.finite(couplr:::BIG_COST))
})
