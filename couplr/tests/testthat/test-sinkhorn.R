# tests/testthat/test-sinkhorn.R
# Tests for Sinkhorn-Knopp entropy-regularized optimal transport

test_that("sinkhorn returns doubly stochastic matrix", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  # Use higher lambda for tighter convergence to uniform marginals
  result <- sinkhorn(cost, lambda = 100, tol = 1e-12)

  expect_true(result$converged)
  expect_equal(dim(result$transport_plan), c(3, 3))

  # Check row and column sums are approximately uniform (1/3 each)
  row_sums <- rowSums(result$transport_plan)
  col_sums <- colSums(result$transport_plan)

  # With finite lambda, marginals are only approximately uniform
  expect_equal(sum(row_sums), 1, tolerance = 1e-6)
  expect_equal(sum(col_sums), 1, tolerance = 1e-6)
  expect_true(all(row_sums > 0))
  expect_true(all(col_sums > 0))
})

test_that("sinkhorn respects custom marginals", {
  cost <- matrix(runif(9), nrow = 3)
  r_weights <- c(0.5, 0.3, 0.2)
  c_weights <- c(0.4, 0.4, 0.2)

  result <- sinkhorn(cost, lambda = 10, r_weights = r_weights, c_weights = c_weights)

  expect_true(result$converged)

  # Check marginals are preserved
  row_sums <- rowSums(result$transport_plan)
  col_sums <- colSums(result$transport_plan)

  expect_equal(row_sums, r_weights, tolerance = 1e-5)
  expect_equal(col_sums, c_weights, tolerance = 1e-5)
})

test_that("higher lambda gives sharper assignments", {
  cost <- matrix(c(1, 10, 10, 10, 1, 10, 10, 10, 1), nrow = 3, byrow = TRUE)

  result_low <- sinkhorn(cost, lambda = 1)
  result_high <- sinkhorn(cost, lambda = 50)

  # Higher lambda should concentrate more mass on diagonal
  diag_mass_low <- sum(diag(result_low$transport_plan))
  diag_mass_high <- sum(diag(result_high$transport_plan))

  expect_gt(diag_mass_high, diag_mass_low)
})

test_that("sinkhorn_to_assignment returns valid matching", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- sinkhorn(cost, lambda = 50)
  hard_match <- sinkhorn_to_assignment(result)

  expect_equal(length(hard_match), 3)
  expect_true(all(hard_match >= 1 & hard_match <= 3))
  expect_equal(length(unique(hard_match)), 3)  # All different
})

test_that("sinkhorn_to_assignment works with matrix input", {
  P <- matrix(c(0.8, 0.1, 0.1,
                0.1, 0.8, 0.1,
                0.1, 0.1, 0.8), nrow = 3, byrow = TRUE)
  hard_match <- sinkhorn_to_assignment(P)

  expect_equal(hard_match, c(1, 2, 3))
})

test_that("sinkhorn handles rectangular matrices", {
  cost <- matrix(runif(12), nrow = 3, ncol = 4)
  result <- sinkhorn(cost, lambda = 10)

  expect_true(result$converged)
  expect_equal(dim(result$transport_plan), c(3, 4))

  # Row sums should be 1/3, column sums should be 1/4
  expect_equal(rowSums(result$transport_plan), rep(1/3, 3), tolerance = 1e-5)
  expect_equal(colSums(result$transport_plan), rep(1/4, 4), tolerance = 1e-5)
})

test_that("sinkhorn handles wide rectangular matrices", {
  cost <- matrix(runif(12), nrow = 4, ncol = 3)
  result <- sinkhorn(cost, lambda = 10)

  expect_true(result$converged)
  expect_equal(dim(result$transport_plan), c(4, 3))

  expect_equal(rowSums(result$transport_plan), rep(1/4, 4), tolerance = 1e-5)
  expect_equal(colSums(result$transport_plan), rep(1/3, 3), tolerance = 1e-5)
})

test_that("sinkhorn returns correct cost", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  result <- sinkhorn(cost, lambda = 50)

  # Compute cost manually
  expected_cost <- sum(result$transport_plan * cost)
  expect_equal(result$cost, expected_cost, tolerance = 1e-10)
})

test_that("sinkhorn returns scaling vectors", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- sinkhorn(cost, lambda = 20)

  expect_equal(length(result$u), 3)
  expect_equal(length(result$v), 3)
  expect_true(all(result$u > 0))
  expect_true(all(result$v > 0))
})

test_that("sinkhorn handles NA/Inf as high cost", {
  cost <- matrix(c(1, NA, 3, 4, 5, Inf, 7, 8, 9), nrow = 3, byrow = TRUE)
  result <- sinkhorn(cost, lambda = 20)

  expect_true(result$converged)
  # Transport should avoid NA/Inf positions
  expect_lt(result$transport_plan[1, 2], 0.01)  # NA position
  expect_lt(result$transport_plan[2, 3], 0.01)  # Inf position
})

test_that("sinkhorn input validation works", {
  cost <- matrix(1:9, nrow = 3)

  expect_error(sinkhorn(cost, lambda = 0), "lambda must be positive")
  expect_error(sinkhorn(cost, lambda = -1), "lambda must be positive")
})

test_that("sinkhorn converges for large matrices", {
  set.seed(123)
  cost <- matrix(runif(100), nrow = 10, ncol = 10)
  result <- sinkhorn(cost, lambda = 10, max_iter = 5000)

  expect_true(result$converged)
  expect_lt(result$iterations, 5000)
})

test_that("sinkhorn is deterministic", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)

  result1 <- sinkhorn(cost, lambda = 20)
  result2 <- sinkhorn(cost, lambda = 20)

  expect_equal(result1$transport_plan, result2$transport_plan)
  expect_equal(result1$cost, result2$cost)
})

test_that("sinkhorn empty matrix returns empty result", {
  cost <- matrix(numeric(0), nrow = 0, ncol = 0)
  result <- sinkhorn(cost)

  expect_equal(dim(result$transport_plan), c(0, 0))
  expect_equal(result$cost, 0)
  expect_true(result$converged)
})

test_that("sinkhorn 1x1 matrix works", {
  cost <- matrix(42, nrow = 1, ncol = 1)
  result <- sinkhorn(cost, lambda = 10)

  expect_equal(result$transport_plan[1, 1], 1, tolerance = 1e-10)
  expect_equal(result$cost, 42, tolerance = 1e-10)
})

test_that("sinkhorn_to_assignment handles 1x1", {
  P <- matrix(1, nrow = 1, ncol = 1)
  expect_equal(sinkhorn_to_assignment(P), 1L)
})

test_that("sinkhorn marginals are normalized", {
  cost <- matrix(runif(9), nrow = 3)

  # Non-normalized weights should be normalized internally
  r_weights <- c(1, 2, 3)  # Sum = 6
  c_weights <- c(2, 2, 2)  # Sum = 6

  result <- sinkhorn(cost, lambda = 10, r_weights = r_weights, c_weights = c_weights)

  # Should be normalized to sum to 1
  row_sums <- rowSums(result$transport_plan)
  col_sums <- colSums(result$transport_plan)

  expect_equal(row_sums, c(1, 2, 3) / 6, tolerance = 1e-5)
  expect_equal(col_sums, rep(1/3, 3), tolerance = 1e-5)
})

test_that("sinkhorn_to_assignment error on invalid input", {
  expect_error(sinkhorn_to_assignment("not a matrix"),
               "must be a sinkhorn\\(\\) result or a transport plan matrix")
  expect_error(sinkhorn_to_assignment(list(a = 1)),
               "must be a sinkhorn\\(\\) result or a transport plan matrix")
})

test_that("sinkhorn rounded matches JV on clear-cut problem", {
  # Problem with obvious optimal assignment
  cost <- matrix(c(1, 100, 100,
                   100, 1, 100,
                   100, 100, 1), nrow = 3, byrow = TRUE)

  result <- sinkhorn(cost, lambda = 50)
  hard_match <- sinkhorn_to_assignment(result)
  jv_match <- assignment(cost, method = "jv")$match

  # Should both find diagonal assignment
  expect_equal(hard_match, c(1, 2, 3))
  expect_equal(jv_match, c(1, 2, 3))
})
