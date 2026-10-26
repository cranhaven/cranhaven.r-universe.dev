# tests/testthat/test-assignment-duals.R
# Tests for assignment_duals() - dual variable extraction

test_that("assignment_duals returns all required components", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_true("match" %in% names(result))
  expect_true("total_cost" %in% names(result))
  expect_true("u" %in% names(result))
  expect_true("v" %in% names(result))
  expect_true("status" %in% names(result))

  expect_equal(length(result$match), 3)
  expect_equal(length(result$u), 3)
  expect_equal(length(result$v), 3)
})

test_that("assignment_duals satisfies complementary slackness", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # For assigned pairs, u[i] + v[j] should equal cost[i,j]
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }
})

test_that("assignment_duals satisfies strong duality", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # sum(u) + sum(v) = total_cost
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-9)
})

test_that("assignment_duals dual feasibility (u + v <= cost)", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # For all (i,j): u[i] + v[j] <= cost[i,j]
  for (i in 1:3) {
    for (j in 1:3) {
      expect_true(result$u[i] + result$v[j] <= cost[i, j] + 1e-9)
    }
  }
})

test_that("assignment_duals matches assignment() result", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)

  result_duals <- assignment_duals(cost)
  result_jv <- assignment(cost, method = "jv")

  expect_equal(result_duals$match, result_jv$match)
  expect_equal(result_duals$total_cost, result_jv$total_cost)
})

test_that("assignment_duals handles rectangular 3x5", {
  cost <- matrix(c(
    1, 5, 9, 2, 6,
    3, 7, 1, 4, 8,
    5, 2, 6, 3, 7
  ), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_equal(length(result$match), 3)
  expect_equal(length(result$u), 3)
  expect_equal(length(result$v), 5)

  # Verify complementary slackness
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }

  # Verify strong duality
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-9)
})

test_that("assignment_duals handles tall rectangular (transposed)", {
  cost <- matrix(c(
    1, 5, 9,
    3, 7, 1,
    5, 2, 6,
    4, 8, 2,
    6, 3, 5
  ), nrow = 5, byrow = TRUE)
  result <- assignment_duals(cost)

  # After transposition: u has length 5, v has length 3
  expect_equal(length(result$u), 5)
  expect_equal(length(result$v), 3)

  # Verify strong duality
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-9)
})

test_that("assignment_duals handles maximization", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost, maximize = TRUE)

  expect_equal(result$status, "optimal")

  # For maximization, the duals are negated
  # So complementary slackness should still hold
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }
})

test_that("assignment_duals handles 1x1", {
  cost <- matrix(42, nrow = 1)
  result <- assignment_duals(cost)

  expect_equal(result$match, 1)
  expect_equal(result$total_cost, 42)
  expect_equal(length(result$u), 1)
  expect_equal(length(result$v), 1)
  expect_equal(result$u[1] + result$v[1], 42)
})

test_that("assignment_duals handles negative costs", {
  cost <- matrix(c(-4, -2, -5, -3, -3, -6, -7, -5, -4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # Verify complementary slackness
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }
})

test_that("assignment_duals handles NA/Inf entries", {
  cost <- matrix(c(
    1, NA, 3,
    5, 6, Inf,
    9, 10, 11
  ), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_equal(result$status, "optimal")

  # Check assigned pairs are finite
  for (i in 1:3) {
    j <- result$match[i]
    expect_true(is.finite(cost[i, j]))
  }
})

test_that("assignment_duals larger matrix", {
  set.seed(123)
  n <- 10
  cost <- matrix(runif(n * n, 1, 100), nrow = n)
  result <- assignment_duals(cost)

  expect_equal(length(result$match), n)
  expect_equal(length(result$u), n)
  expect_equal(length(result$v), n)

  # Verify strong duality
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-6)

  # Verify complementary slackness
  for (i in 1:n) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-6)
  }
})

test_that("assignment_duals reduced costs are non-negative", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # Reduced cost = cost - u - v >= 0 for all (i,j)
  for (i in 1:3) {
    for (j in 1:3) {
      reduced_cost <- cost[i, j] - result$u[i] - result$v[j]
      expect_true(reduced_cost >= -1e-9)
    }
  }
})

test_that("assignment_duals print method works", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_output(print(result), "Assignment Result with Duals")
  expect_output(print(result), "Dual variables")
})

test_that("assignment_duals class is correct", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- assignment_duals(cost)

  expect_s3_class(result, "assignment_duals_result")
})

test_that("assignment_duals zero cost diagonal", {
  cost <- matrix(c(0, 1, 2, 3, 0, 4, 5, 6, 0), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_equal(result$total_cost, 0)
  expect_equal(result$match, c(1, 2, 3))
})

test_that("assignment_duals sensitivity analysis example", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # Compute reduced costs (sensitivity to cost changes)
  reduced <- matrix(NA, 3, 3)
  for (i in 1:3) {
    for (j in 1:3) {
      reduced[i, j] <- cost[i, j] - result$u[i] - result$v[j]
    }
  }

  # Assigned pairs have zero reduced cost
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(reduced[i, j], 0, tolerance = 1e-9)
  }

  # Non-assigned pairs have positive reduced cost
  for (i in 1:3) {
    for (j in 1:3) {
      if (result$match[i] != j) {
        expect_true(reduced[i, j] > -1e-9)
      }
    }
  }
})

# ==============================================================================
# certify = TRUE, and the lazy dual entry point
# ==============================================================================

test_that("certify = TRUE attaches a certificate the duals prove", {
  set.seed(1)
  cost <- matrix(runif(120), 6, 20)

  plain <- assignment_duals(cost)
  certified <- assignment_duals(cost, certify = TRUE)

  expect_null(plain$certificate)
  expect_s3_class(certified$certificate, "assignment_certificate")
  expect_true(certified$certificate$certified_optimal)
  expect_equal(certified$certificate$duality_gap, 0, tolerance = 1e-9)

  # The certificate is the only added field.
  expect_identical(unclass(certified)[names(plain)], unclass(plain))
})

test_that("certify = TRUE covers the square and transposed orientations", {
  set.seed(2)

  square <- assignment_duals(matrix(runif(100), 10, 10), certify = TRUE)
  expect_true(square$certificate$certified_optimal)
  expect_false(square$certificate$transposed)

  tall <- assignment_duals(matrix(runif(120), 20, 6), certify = TRUE)
  expect_true(tall$certificate$certified_optimal)
  expect_true(tall$certificate$transposed)

  maxi <- assignment_duals(matrix(runif(120), 6, 20), maximize = TRUE,
                           certify = TRUE)
  expect_true(maxi$certificate$certified_optimal)
})

test_that("certify rejects a non-flag", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  expect_error(assignment_duals(cost, certify = NA), "`certify` must be")
  expect_error(assignment_duals(cost, certify = c(TRUE, TRUE)), "`certify` must be")
})

test_that("assignment_duals solves a lazy specification without materializing it", {
  set.seed(42)
  left  <- data.frame(id = 1:8,  x = rnorm(8),  y = rnorm(8))
  right <- data.frame(id = 9:20, x = rnorm(12), y = rnorm(12))

  spec  <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")
  dense <- compute_distance_matrix(as.matrix(left[, c("x", "y")]),
                                   as.matrix(right[, c("x", "y")]),
                                   distance = "euclidean")

  lazy_duals  <- assignment_duals(spec,  certify = TRUE)
  dense_duals <- assignment_duals(dense, certify = TRUE)

  expect_identical(lazy_duals$match, dense_duals$match)
  expect_equal(lazy_duals$total_cost, dense_duals$total_cost, tolerance = 1e-9)
  expect_equal(lazy_duals$u, dense_duals$u, tolerance = 1e-9)
  expect_equal(lazy_duals$v, dense_duals$v, tolerance = 1e-9)
  expect_true(lazy_duals$certificate$certified_optimal)
})

test_that("the lazy dual path transposes when there are more left units than right", {
  set.seed(7)
  left  <- data.frame(id = 1:12,  x = rnorm(12), y = rnorm(12))
  right <- data.frame(id = 13:20, x = rnorm(8),  y = rnorm(8))

  spec  <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")
  dense <- compute_distance_matrix(as.matrix(left[, c("x", "y")]),
                                   as.matrix(right[, c("x", "y")]),
                                   distance = "euclidean")

  lazy_duals  <- assignment_duals(spec,  certify = TRUE)
  dense_duals <- assignment_duals(dense, certify = TRUE)

  expect_identical(lazy_duals$match, dense_duals$match)
  expect_equal(lazy_duals$total_cost, dense_duals$total_cost, tolerance = 1e-9)
  expect_true(lazy_duals$certificate$certified_optimal)
  expect_true(lazy_duals$certificate$transposed)
})

test_that("verify_assignment derives duals for a lazy specification", {
  set.seed(11)
  left  <- data.frame(id = 1:6,  x = rnorm(6),  y = rnorm(6))
  right <- data.frame(id = 7:20, x = rnorm(14), y = rnorm(14))
  spec  <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")

  res <- assignment(spec)
  expect_true(verify_assignment(res, spec)$certified_optimal)

  # A matching that is not optimal fails the same check.
  scrambled <- res
  scrambled$match <- rev(res$match)
  expect_false(verify_assignment(scrambled, spec)$certified_optimal)
})
