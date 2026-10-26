# ==============================================================================
# Tests for CSA (Cost Scaling Algorithm) solver
# ==============================================================================

# The CSA solver is the Goldberg-Kennedy cost-scaling algorithm,
# which is often fastest for medium-large problems.

# ------------------------------------------------------------------------------
# Basic functionality tests
# ------------------------------------------------------------------------------

test_that("assignment with method='csa' works on simple problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "csa")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(length(result$match), 2)
  expect_equal(result$status, "optimal")
  expect_equal(result$method_used, "csa")
})

test_that("csa finds optimal assignment for 3x3 problem", {
  cost <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "csa")

  # Optimal assignment: (1,1), (2,2), (3,3) or (1,3), (2,2), (3,1)
  expect_equal(result$total_cost, 15)  # 1 + 5 + 9 = 15
  expect_equal(result$status, "optimal")
})

test_that("csa handles rectangular matrices (more cols)", {
  cost <- matrix(c(
    1, 2, 3, 4,
    5, 6, 7, 8
  ), nrow = 2, byrow = TRUE)

  result <- assignment(cost, method = "csa")

  expect_equal(length(result$match), 2)
  expect_true(result$total_cost <= 3 + 8)  # At most col 1 + col 4
})

test_that("csa handles rectangular matrices (more rows)", {
  cost <- matrix(c(
    1, 2,
    3, 4,
    5, 6
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "csa")

  # With auto-transpose, should work
  expect_equal(length(result$match), 3)
})

# ------------------------------------------------------------------------------
# Maximization tests
# ------------------------------------------------------------------------------

test_that("csa works with maximize=TRUE", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "csa", maximize = TRUE)

  expect_equal(result$total_cost, 10)  # 5 + 5
})

test_that("csa maximization finds correct assignment", {
  cost <- matrix(c(
    1, 9, 3,
    4, 2, 6,
    7, 5, 8
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "csa", maximize = TRUE)

  # Maximum: 9 + 6 + 7 = 22 or similar
  expect_true(result$total_cost >= 20)
  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("csa handles 1x1 matrix", {
  cost <- matrix(5, 1, 1)

  result <- assignment(cost, method = "csa")

  expect_equal(result$match, 1L)
  expect_equal(result$total_cost, 5)
})

test_that("csa handles identity cost matrix", {
  n <- 4
  cost <- diag(n)  # Identity: diagonal = 1, off-diagonal = 0

  result <- assignment(cost, method = "csa")

  # Optimal is to avoid the diagonal (all zeros)
  expect_equal(result$total_cost, 0)
})

test_that("csa handles uniform cost matrix", {
  cost <- matrix(5, 3, 3)

  result <- assignment(cost, method = "csa")

  expect_equal(result$total_cost, 15)  # 3 * 5
})

test_that("csa handles NA (forbidden) entries", {
  cost <- matrix(c(
    1, NA, 3,
    4, 5, NA,
    NA, 8, 9
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "csa")

  # Should find valid assignment avoiding NAs
  expect_equal(result$status, "optimal")
})

test_that("csa handles Inf (forbidden) entries", {
  cost <- matrix(c(
    1, Inf, 3,
    4, 5, Inf,
    Inf, 8, 9
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "csa")

  # Should find valid assignment avoiding Infs
  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Larger problem tests
# ------------------------------------------------------------------------------

test_that("csa handles medium-size problems efficiently", {
  skip_on_cran()  # Skip time-sensitive tests on CRAN

  set.seed(123)
  n <- 50
  cost <- matrix(runif(n * n), n, n)

  result <- assignment(cost, method = "csa")

  expect_equal(length(result$match), n)
  expect_equal(result$status, "optimal")
  # Should be a valid permutation
  expect_equal(sort(result$match), 1:n)
})

test_that("csa produces valid permutation", {
  set.seed(456)
  n <- 20
  cost <- matrix(runif(n * n), n, n)

  result <- assignment(cost, method = "csa")

  # Match should be a permutation (each column assigned exactly once)
  expect_equal(sort(result$match), 1:n)
})

# ------------------------------------------------------------------------------
# Comparison with other solvers
# ------------------------------------------------------------------------------
# NOTE - We don't assume identical solutions from different solvers
# as tie-breaking can differ. But optimal costs should match.

test_that("csa matches hungarian solver on optimal cost", {
  set.seed(789)
  cost <- matrix(runif(16), 4, 4)

  result_csa <- assignment(cost, method = "csa")
  result_hun <- assignment(cost, method = "hungarian")

  # Optimal costs should be equal (or very close due to floating point)
  expect_equal(result_csa$total_cost, result_hun$total_cost, tolerance = 1e-9)
})

test_that("csa matches jv solver on optimal cost", {
  set.seed(101)
  cost <- matrix(runif(25), 5, 5)

  result_csa <- assignment(cost, method = "csa")
  result_jv <- assignment(cost, method = "jv")

  expect_equal(result_csa$total_cost, result_jv$total_cost, tolerance = 1e-9)
})

# ------------------------------------------------------------------------------
# Stress tests
# ------------------------------------------------------------------------------

test_that("csa handles sparse matrix (many forbidden entries)", {
  n <- 10
  cost <- matrix(Inf, n, n)
  # Create a valid assignment path
  for (i in 1:n) {
    cost[i, i] <- runif(1)
    cost[i, (i %% n) + 1] <- runif(1)
  }

  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_true(is.finite(result$total_cost))
})

test_that("csa handles wide value range", {
  cost <- matrix(c(
    1e-6, 1e6,
    1e6, 1e-6
  ), 2, 2)

  result <- assignment(cost, method = "csa")

  expect_equal(result$total_cost, 2e-6, tolerance = 1e-12)
})

# ------------------------------------------------------------------------------
# Optimality regression tests
#
# csa was previously suboptimal on (a) fractional costs, because its
# epsilon-scaling termination only guarantees optimality for integer costs, and
# (b) rectangular problems, because without padding to a square problem the
# auction has no price competition over the surplus columns. Both are checked
# here against an independent optimal solver.
# ------------------------------------------------------------------------------

test_that("csa is optimal on fractional-cost matrices", {
  set.seed(1234)
  for (i in seq_len(150)) {
    n <- sample(3:9, 1)
    cost <- matrix(runif(n * n) * 1e-3, n, n)  # tiny gaps stress eps-optimality
    ref <- assignment(cost, method = "jv")$total_cost
    csa <- assignment(cost, method = "csa")$total_cost
    expect_equal(csa, ref, tolerance = 1e-9,
                 info = paste("fractional seed iter", i))
  }
})

test_that("csa is optimal on rectangular matrices (n < m)", {
  set.seed(56)
  for (i in seq_len(120)) {
    n <- sample(3:5, 1)
    m <- n + sample(1:3, 1)
    maximize <- (i %% 2 == 0)
    cost <- matrix(sample(1:40, n * m, replace = TRUE), n, m)
    ref <- assignment(cost, method = "bruteforce", maximize = maximize)$total_cost
    csa <- assignment(cost, method = "csa", maximize = maximize)$total_cost
    expect_equal(csa, ref, tolerance = 1e-9,
                 info = paste("rectangular iter", i, "maximize", maximize))
  }
})

test_that("csa matches jv with forbidden edges", {
  set.seed(77)
  for (i in seq_len(120)) {
    n <- sample(3:6, 1)
    m <- sample(n:(n + 2), 1)
    cost <- matrix(sample(1:30, n * m, replace = TRUE), n, m)
    nf <- sample(0:2, 1)
    if (nf > 0) cost[sample(seq_len(n * m), nf)] <- Inf
    ref <- tryCatch(assignment(cost, method = "jv")$total_cost,
                    error = function(e) NA_real_)
    csa <- tryCatch(assignment(cost, method = "csa")$total_cost,
                    error = function(e) NA_real_)
    # Agree on both feasibility and value
    expect_equal(is.na(ref), is.na(csa), info = paste("forbidden iter", i))
    if (!is.na(ref)) {
      expect_equal(csa, ref, tolerance = 1e-9, info = paste("forbidden iter", i))
    }
  }
})

# ------------------------------------------------------------------------------
# Via lap_solve interface
# ------------------------------------------------------------------------------

test_that("lap_solve works with csa method", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve(cost, method = "csa")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(nrow(result), 2)
  expect_true("cost" %in% names(result))
})
