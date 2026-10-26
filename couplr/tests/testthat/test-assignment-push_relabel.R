# tests/testthat/test-assignment-push_relabel.R
# Tests for Push-Relabel assignment solver

test_that("push_relabel solves square 3x3 correctly", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 3)
  expect_true(all(res$match >= 1 & res$match <= 3))
  expect_equal(length(unique(res$match)), 3)

  # Verify cost matches JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel solves rectangular 3x5 (n < m)", {
  skip_on_cran()
  cost <- matrix(c(
    1, 5, 9, 2, 6,
    3, 7, 1, 4, 8,
    5, 2, 6, 3, 7
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 3)
  expect_true(all(res$match >= 1 & res$match <= 5))
  expect_equal(length(unique(res$match)), 3)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel handles wide rectangular 3x10", {
  skip_on_cran()
  set.seed(42)
  cost <- matrix(runif(30), nrow = 3, ncol = 10)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 3)
  expect_true(all(res$match >= 1 & res$match <= 10))
  expect_equal(length(unique(res$match)), 3)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost, tolerance = 1e-9)
})

test_that("push_relabel handles tall rectangular (transposed)", {
  skip_on_cran()
  # 5 rows, 3 cols - will be auto-transposed
  cost <- matrix(c(
    1, 5, 9,
    3, 7, 1,
    5, 2, 6,
    4, 8, 2,
    6, 3, 5
  ), nrow = 5, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), 5)
  expect_equal(sum(res$match > 0), 3)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel handles NA entries", {
  skip_on_cran()
  cost <- matrix(c(
    1, NA, 3, 4,
    5, 6, NA, 8,
    9, 10, 11, 12
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

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

test_that("push_relabel handles Inf entries", {
  skip_on_cran()
  cost <- matrix(c(
    1, Inf, 3, 4,
    5, 6, Inf, 8,
    9, 10, 11, 12
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

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

test_that("push_relabel handles maximization", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel", maximize = TRUE)

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv", maximize = TRUE)
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel handles single row", {
  skip_on_cran()
  cost <- matrix(c(5, 2, 8, 1), nrow = 1)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(res$match, 4)  # min is at col 4
  expect_equal(res$total_cost, 1)
})

test_that("push_relabel handles 1x1", {
  skip_on_cran()
  cost <- matrix(42, nrow = 1)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$match, 1)
  expect_equal(res$total_cost, 42)
})

test_that("push_relabel handles larger square matrix", {
  skip_on_cran()
  set.seed(123)
  n <- 20
  cost <- matrix(runif(n * n, 1, 100), nrow = n)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(length(res$match), n)
  expect_equal(length(unique(res$match)), n)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost, tolerance = 1e-6)
})

test_that("push_relabel handles negative costs", {
  skip_on_cran()
  cost <- matrix(c(-4, -2, -5, -3, -3, -6, -7, -5, -4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel handles mixed positive/negative costs", {
  skip_on_cran()
  cost <- matrix(c(-4, 2, -5, 3, -3, 6, -7, 5, -4), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel handles sparse rectangular", {
  skip_on_cran()
  # 4x8 with many Infs
  cost <- matrix(Inf, nrow = 4, ncol = 8)
  cost[1, c(1, 3)] <- c(1, 5)
  cost[2, c(2, 4, 6)] <- c(2, 6, 10)
  cost[3, c(3, 5, 7)] <- c(3, 7, 11)
  cost[4, c(4, 6, 8)] <- c(4, 8, 12)

  res <- assignment(cost, method = "push_relabel")

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

test_that("push_relabel handles all same costs", {
  skip_on_cran()
  cost <- matrix(5, nrow = 3, ncol = 5)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 15)  # 3 rows * 5 each
  expect_equal(length(unique(res$match)), 3)
})

test_that("push_relabel handles diagonal optimal", {
  skip_on_cran()
  cost <- diag(1, 4, 6)  # 4x6 with 1s on diagonal, 0s elsewhere
  cost[cost == 0] <- 10  # Make off-diagonal expensive
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 4)
  expect_equal(res$match, c(1, 2, 3, 4))
})

test_that("push_relabel computes correct assignment cost", {
  skip_on_cran()
  cost <- matrix(c(
    3, 7, 2, 9,
    5, 1, 8, 4,
    9, 3, 6, 2
  ), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  # Manually verify the cost
  computed_cost <- sum(cost[cbind(1:3, res$match)])
  expect_equal(res$total_cost, computed_cost)

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel errors on infeasible problem", {
  skip_on_cran()
  # Row 1 has no valid assignments
  cost <- matrix(c(Inf, Inf, Inf, 1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)

  expect_error(assignment(cost, method = "push_relabel"), "Infeasible")
})

test_that("push_relabel handles integer costs", {
  skip_on_cran()
  cost <- matrix(as.integer(c(4, 2, 5, 3, 3, 6, 7, 5, 4)), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost)
})

test_that("push_relabel handles 2x2 matrix", {
  skip_on_cran()
  cost <- matrix(c(1, 3, 2, 4), nrow = 2)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 5)
})

test_that("push_relabel benchmark vs JV on 30x30", {
  skip_on_cran()
  set.seed(789)
  cost <- matrix(runif(900, 1, 100), nrow = 30, ncol = 30)

  res_pr <- assignment(cost, method = "push_relabel")
  res_jv <- assignment(cost, method = "jv")

  expect_equal(res_pr$total_cost, res_jv$total_cost, tolerance = 1e-6)
})

test_that("push_relabel handles repeated values in row", {
  skip_on_cran()
  cost <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 3), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 6)  # 1+2+3
  expect_equal(length(unique(res$match)), 3)
})

test_that("push_relabel method is reported correctly", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$method_used, "push_relabel")
})

test_that("push_relabel handles zero costs", {
  skip_on_cran()
  cost <- matrix(c(0, 1, 2, 3, 0, 4, 5, 6, 0), nrow = 3, byrow = TRUE)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")
  expect_equal(res$total_cost, 0)
  expect_equal(res$match, c(1, 2, 3))
})

test_that("push_relabel handles very small costs", {
  skip_on_cran()
  cost <- matrix(c(1e-10, 2e-10, 3e-10, 4e-10), nrow = 2)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost, tolerance = 1e-15)
})

test_that("push_relabel handles very large costs", {
  skip_on_cran()
  cost <- matrix(c(1e10, 2e10, 3e10, 4e10), nrow = 2)
  res <- assignment(cost, method = "push_relabel")

  expect_equal(res$status, "optimal")

  # Verify against JV
  res_jv <- assignment(cost, method = "jv")
  expect_equal(res$total_cost, res_jv$total_cost, tolerance = 1e5)
})
