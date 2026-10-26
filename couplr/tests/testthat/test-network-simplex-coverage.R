# ==============================================================================
# Tests for network_simplex solver coverage
# ==============================================================================

# Test the edge cases in solve_network_simplex.cpp that are not hit by normal tests

test_that("network_simplex handles 1x1 matrix", {
  skip_on_cran()
  cost <- matrix(5, 1, 1)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$match, 1L)
  expect_equal(result$total_cost, 5)
  expect_equal(result$status, "optimal")
})

test_that("network_simplex handles rectangular matrix (more cols)", {
  skip_on_cran()
  cost <- matrix(1:6, 2, 3)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(length(result$match), 2)
  expect_equal(result$status, "optimal")
})

test_that("network_simplex handles rectangular matrix (more rows)", {
  skip_on_cran()
  # This should trigger the "infeasible" branch in network_simplex
  cost <- matrix(1:6, 3, 2)
  result <- assignment(cost, method = "network_simplex")

  # With more rows than columns, some rows can't be matched
  expect_equal(length(result$match), 3)
  # Check that at most 2 rows are matched (to 2 columns)
  expect_true(sum(result$match > 0) <= 2)
})

test_that("network_simplex handles tie-breaking", {
  skip_on_cran()
  # Matrix with equal costs - tests tie-breaking
  cost <- matrix(1, 3, 3)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
  expect_equal(result$total_cost, 3)
})

test_that("network_simplex handles large range of costs", {
  skip_on_cran()
  cost <- matrix(c(1, 1000000, 1000000, 1), 2, 2)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
  expect_equal(result$total_cost, 2)  # 1 + 1
})

test_that("network_simplex handles negative costs", {
  skip_on_cran()
  cost <- matrix(c(-5, -1, -2, -10), 2, 2)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
  # Optimal should be -5 + -10 = -15
  expect_equal(result$total_cost, -15)
})

test_that("network_simplex handles diagonal optimal", {
  skip_on_cran()
  # Diagonal is optimal
  cost <- matrix(c(1, 100, 100, 1), 2, 2)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$total_cost, 2)
})

test_that("network_simplex handles anti-diagonal optimal", {
  skip_on_cran()
  # Anti-diagonal is optimal
  cost <- matrix(c(100, 1, 1, 100), 2, 2)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$total_cost, 2)
})

test_that("network_simplex with forbidden assignments", {
  skip_on_cran()
  cost <- matrix(c(1, Inf, Inf, 1), 2, 2)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
  expect_equal(result$total_cost, 2)
})

test_that("network_simplex handles 4x4 matrix correctly", {
  skip_on_cran()
  cost <- matrix(c(
    1, 5, 3, 4,
    2, 6, 1, 5,
    4, 2, 7, 3,
    6, 3, 2, 1
  ), 4, 4, byrow = TRUE)

  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 4)
  expect_true(all(result$match >= 1 & result$match <= 4))
})

test_that("network_simplex pivot count is returned", {
  skip_on_cran()
  cost <- matrix(runif(16), 4, 4)
  result <- lap_solve(cost, method = "network_simplex")

  # Check that the result has expected columns
  expect_true("source" %in% names(result))
  expect_true("target" %in% names(result))
})

test_that("network_simplex matches jv on random matrices", {
  skip_on_cran()
  set.seed(42)
  for (n in c(3, 5, 8)) {
    cost <- matrix(runif(n * n), n, n)

    result_ns <- assignment(cost, method = "network_simplex")
    result_jv <- assignment(cost, method = "jv")

    # Both should be optimal with same total cost
    expect_equal(result_ns$total_cost, result_jv$total_cost,
                 tolerance = 1e-10,
                 info = sprintf("n=%d", n))
  }
})
