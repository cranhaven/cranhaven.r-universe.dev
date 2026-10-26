# tests/testthat/test-assignment-ramshaw_tarjan.R
# Tests for Ramshaw-Tarjan rectangular assignment solver

test_that("ramshaw_tarjan solves square 3x3 correctly", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 3)
  expect_true(all(res$match >= 1 & res$match <= 3))
  expect_equal(length(unique(res$match)), 3)

  # Verify cost matches JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan solves rectangular 3x5 (n < m)", {
  cost <- matrix(c(
    1, 5, 9, 2, 6,
    3, 7, 1, 4, 8,
    5, 2, 6, 3, 7
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 3)
  expect_true(all(res$match >= 1 & res$match <= 5))
  expect_equal(length(unique(res$match)), 3)  # All different columns

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles wide rectangular 3x10", {
  set.seed(42)
  cost <- matrix(runif(30), nrow = 3, ncol = 10)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 3)
  expect_true(all(res$match >= 1 & res$match <= 10))
  expect_equal(length(unique(res$match)), 3)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost, tolerance = 1e-9)
})

test_that("ramshaw_tarjan handles tall rectangular (transposed)", {
  # 5 rows, 3 cols - will be auto-transposed
  cost <- matrix(c(
    1, 5, 9,
    3, 7, 1,
    5, 2, 6,
    4, 8, 2,
    6, 3, 5
  ), nrow = 5, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  # Match vector has length nrow, with 0s for unmatched rows
  expect_equal(length(res$match), 5)
  # Only 3 rows can be matched (to 3 columns)
  expect_equal(sum(res$match > 0), 3)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles NA entries", {
  cost <- matrix(c(
    1, NA, 3, 4,
    5, 6, NA, 8,
    9, 10, 11, 12
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 3)

  # Check no forbidden assignments
  for (i in 1:3) {
    j <- res$match[i]
    expect_true(is.finite(cost[i, j]))
  }

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles Inf entries", {
  cost <- matrix(c(
    1, Inf, 3, 4,
    5, 6, Inf, 8,
    9, 10, 11, 12
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")

  # Check no forbidden assignments
  for (i in seq_along(res$match)) {
    j <- res$match[i]
    if (j > 0) {
      expect_true(is.finite(cost[i, j]))
    }
  }

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles maximization", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan", maximize = TRUE)

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv", maximize = TRUE)
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles single row", {
  cost <- matrix(c(5, 2, 8, 1), nrow = 1)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(res$match, 4)  # min is at col 4
  expect_equal(res$total_cost, 1)
})

test_that("ramshaw_tarjan handles 1x1", {
  cost <- matrix(42, nrow = 1)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$match, 1)
  expect_equal(res$total_cost, 42)
})

test_that("ramshaw_tarjan handles larger square matrix", {
  set.seed(123)
  n <- 20
  cost <- matrix(runif(n * n, 1, 100), nrow = n)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), n)
  expect_equal(length(unique(res$match)), n)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost, tolerance = 1e-6)
})

test_that("ramshaw_tarjan handles very rectangular 5x50", {
  set.seed(456)
  cost <- matrix(runif(250), nrow = 5, ncol = 50)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 5)
  expect_equal(length(unique(res$match)), 5)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost, tolerance = 1e-9)
})

test_that("ramshaw_tarjan handles negative costs", {
  cost <- matrix(c(-4, -2, -5, -3, -3, -6, -7, -5, -4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles mixed positive/negative costs", {
  cost <- matrix(c(-4, 2, -5, 3, -3, 6, -7, 5, -4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles sparse rectangular", {
  # 4x8 with many Infs
  cost <- matrix(Inf, nrow = 4, ncol = 8)
  cost[1, c(1, 3)] <- c(1, 5)
  cost[2, c(2, 4, 6)] <- c(2, 6, 10)
  cost[3, c(3, 5, 7)] <- c(3, 7, 11)
  cost[4, c(4, 6, 8)] <- c(4, 8, 12)

  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 4)

  # All assignments must be finite
  for (i in 1:4) {
    expect_true(is.finite(cost[i, res$match[i]]))
  }

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles all same costs", {
  cost <- matrix(5, nrow = 3, ncol = 5)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 15)  # 3 rows * 5 each
  expect_equal(length(unique(res$match)), 3)
})

test_that("ramshaw_tarjan handles diagonal optimal", {
  cost <- diag(1, 4, 6)  # 4x6 with 1s on diagonal, 0s elsewhere
  cost[cost == 0] <- 10  # Make off-diagonal expensive
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  # Should pick diagonal (cost 1 each)
  expect_equal(res$total_cost, 4)
  expect_equal(res$match, c(1, 2, 3, 4))
})

test_that("ramshaw_tarjan computes correct assignment cost", {
  cost <- matrix(c(
    3, 7, 2, 9,
    5, 1, 8, 4,
    9, 3, 6, 2
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  # Manually verify the cost
  computed_cost <- sum(cost[cbind(1:3, res$match)])
  expect_equal(res$total_cost, computed_cost)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan errors on infeasible problem", {
  # Row 1 has no valid assignments
  cost <- matrix(c(Inf, Inf, Inf, 1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)

  expect_error(assignment(cost, method = "ramshaw_tarjan"), "no valid")
})

test_that("ramshaw_tarjan handles integer costs", {
  cost <- matrix(as.integer(c(4, 2, 5, 3, 3, 6, 7, 5, 4)), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("ramshaw_tarjan handles 2x2 matrix", {
  cost <- matrix(c(1, 3, 2, 4), nrow = 2)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  # Optimal: (1,1)=1 and (2,2)=4 -> 5
  # Or: (1,2)=2 and (2,1)=3 -> 5
  expect_equal(res$total_cost, 5)
})

test_that("ramshaw_tarjan benchmark vs JV on 10x100", {
  set.seed(789)
  cost <- matrix(runif(1000, 1, 100), nrow = 10, ncol = 100)

  res_rt <- assignment(cost, method = "ramshaw_tarjan")
  res_jv <- assignment(cost, method = "jv")

  expect_equal(res_rt$total_cost, res_jv$total_cost, tolerance = 1e-6)
})

test_that("ramshaw_tarjan handles repeated values in row", {
  cost <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 6)  # 1+2+3
  expect_equal(length(unique(res$match)), 3)
})

test_that("ramshaw_tarjan method is reported correctly", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$method_used, "ramshaw_tarjan")
})

test_that("ramshaw_tarjan handles zero costs", {
  cost <- matrix(c(0, 1, 2, 3, 0, 4, 5, 6, 0), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 0)  # Can pick all zeros on diagonal
  expect_equal(res$match, c(1, 2, 3))
})
