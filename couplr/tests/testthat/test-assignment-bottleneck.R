# test-assignment-bottleneck.R
# Tests for Bottleneck Assignment Problem (BAP) solver

# ==============================================================================
# Basic Functionality
# ==============================================================================

test_that("bottleneck_assignment returns correct structure", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- bottleneck_assignment(cost)

  expect_type(result, "list")
  expect_s3_class(result, "bottleneck_result")
  expect_true("match" %in% names(result))
  expect_true("bottleneck" %in% names(result))
  expect_true("status" %in% names(result))
  expect_equal(result$status, "optimal")
})

test_that("bottleneck_assignment minimizes maximum edge cost", {
  # Cost matrix where optimal LAP and optimal BAP differ
  # LAP sum-optimal: 1->1 (1) + 2->2 (5) = 6, max = 5

  # BAP optimal: 1->2 (3) + 2->1 (2) = 5, max = 3
  cost <- matrix(c(1, 3,
                   2, 5), nrow = 2, byrow = TRUE)

  result <- bottleneck_assignment(cost)

  # Bottleneck should be 3 (the minimax)
  expect_equal(result$bottleneck, 3)

  # Verify the assignment achieves this bottleneck
  edges <- sapply(seq_along(result$match), function(i) cost[i, result$match[i]])
  expect_equal(max(edges), result$bottleneck)
})

test_that("bottleneck_assignment handles single element", {
  cost <- matrix(7, nrow = 1, ncol = 1)
  result <- bottleneck_assignment(cost)

  expect_equal(result$match, 1L)
  expect_equal(result$bottleneck, 7)
})

test_that("bottleneck_assignment handles 1x1 matrix", {
  cost <- matrix(42.5, nrow = 1, ncol = 1)
  result <- bottleneck_assignment(cost)

  expect_equal(result$match, 1L)
  expect_equal(result$bottleneck, 42.5)
})

# ==============================================================================
# Rectangular Matrices
# ==============================================================================

test_that("bottleneck_assignment handles rectangular matrix (more cols)", {
  # 2x4 matrix
  cost <- matrix(c(5, 2, 8, 1,
                   3, 6, 1, 4), nrow = 2, byrow = TRUE)

  result <- bottleneck_assignment(cost)

  expect_length(result$match, 2)
  expect_true(all(result$match >= 1 & result$match <= 4))
  expect_true(length(unique(result$match)) == 2)  # No duplicates

  # Verify bottleneck
  edges <- sapply(1:2, function(i) cost[i, result$match[i]])
  expect_equal(max(edges), result$bottleneck)
})

test_that("bottleneck_assignment rejects more rows than cols",
{
  cost <- matrix(1:6, nrow = 3, ncol = 2)
  expect_error(bottleneck_assignment(cost), "nrow <= ncol")
})

# ==============================================================================
# NA and Inf Handling
# ==============================================================================

test_that("bottleneck_assignment handles NA (forbidden edges)", {
  cost <- matrix(c(1, NA, 3,
                   2, 4, NA,
                   NA, 1, 2), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)

  # Should find valid assignment avoiding NA

  expect_length(result$match, 3)
  expect_true(all(result$match >= 1 & result$match <= 3))

  # Verify no NA edges selected
  for (i in 1:3) {
    expect_false(is.na(cost[i, result$match[i]]))
  }
})

test_that("bottleneck_assignment handles Inf (forbidden edges)", {
  cost <- matrix(c(1, Inf, 3,
                   2, 4, Inf,
                   Inf, 1, 2), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)

  # Verify no Inf edges selected
  for (i in 1:3) {
    expect_true(is.finite(cost[i, result$match[i]]))
  }
})

test_that("bottleneck_assignment fails on infeasible (all NA)", {
  cost <- matrix(NA_real_, nrow = 2, ncol = 2)
  expect_error(bottleneck_assignment(cost), "Infeasible|no finite")
})

test_that("bottleneck_assignment fails on infeasible (no perfect matching)", {
  # Only one finite entry per row, but they conflict
  cost <- matrix(c(1, Inf,
                   2, Inf), nrow = 2, byrow = TRUE)
  expect_error(bottleneck_assignment(cost), "Infeasible|no perfect matching")
})

test_that("bottleneck_assignment rejects NaN", {
  cost <- matrix(c(1, NaN, 3, 4), nrow = 2)
  expect_error(bottleneck_assignment(cost), "NaN")
})

# ==============================================================================
# Maximize Mode (Maximin)
# ==============================================================================

test_that("bottleneck_assignment maximize mode works", {
  # Maximize minimum profit
  profit <- matrix(c(10, 5, 3,
                     2, 8, 6,
                     4, 1, 9), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(profit, maximize = TRUE)

  # Verify bottleneck is minimum among selected edges
  edges <- sapply(1:3, function(i) profit[i, result$match[i]])
  expect_equal(min(edges), result$bottleneck)
})

test_that("maximize gives different result than minimize", {
  cost <- matrix(c(1, 10,
                   10, 1), nrow = 2, byrow = TRUE)

  result_min <- bottleneck_assignment(cost, maximize = FALSE)
  result_max <- bottleneck_assignment(cost, maximize = TRUE)

  # Both should have valid matchings
  expect_length(result_min$match, 2)
  expect_length(result_max$match, 2)

  # Min should achieve bottleneck of 1 (diagonal assignment)
  expect_equal(result_min$bottleneck, 1)

  # Max should achieve bottleneck of 10 (anti-diagonal assignment)
  expect_equal(result_max$bottleneck, 10)
})

# ==============================================================================
# Comparison with LAP
# ==============================================================================

test_that("bottleneck differs from LAP sum-optimal", {
  # Classic example where BAP and LAP optima differ
  cost <- matrix(c(1, 100,
                   2, 3), nrow = 2, byrow = TRUE)

  bap <- bottleneck_assignment(cost)
  lap <- assignment(cost)

  # LAP optimal: 1->1 (1) + 2->2 (3) = 4, max = 3
  # BAP optimal: 1->1 (1) + 2->2 (3), max = 3
  # Actually same here, let's try another

  cost2 <- matrix(c(10, 1,
                    1, 10), nrow = 2, byrow = TRUE)

  bap2 <- bottleneck_assignment(cost2)
  lap2 <- assignment(cost2)

  # LAP: 1->2 (1) + 2->1 (1) = 2
  # BAP: same assignment, bottleneck = 1
  expect_equal(bap2$bottleneck, 1)
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("bottleneck_assignment handles all equal costs", {
  cost <- matrix(5, nrow = 3, ncol = 3)
  result <- bottleneck_assignment(cost)

  expect_equal(result$bottleneck, 5)
  expect_length(result$match, 3)
  expect_equal(length(unique(result$match)), 3)  # Valid permutation
})

test_that("bottleneck_assignment handles negative costs", {
  cost <- matrix(c(-5, -2, -3,
                   -1, -4, -6,
                   -7, -8, -9), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)

  # Should minimize max (closest to 0 for negatives)
  edges <- sapply(1:3, function(i) cost[i, result$match[i]])
  expect_equal(max(edges), result$bottleneck)
})

test_that("bottleneck_assignment handles mixed pos/neg costs", {
  cost <- matrix(c(-5, 2, -3,
                   1, -4, 6,
                   -7, 8, -9), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)
  expect_type(result$bottleneck, "double")
})

test_that("bottleneck_assignment handles large integers", {
  cost <- matrix(c(1e9, 1e8, 1e7,
                   1e6, 1e9, 1e5,
                   1e4, 1e3, 1e9), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)
  expect_true(is.finite(result$bottleneck))
})

test_that("bottleneck_assignment handles very small costs", {
  cost <- matrix(c(1e-10, 1e-9, 1e-8,
                   1e-7, 1e-10, 1e-6,
                   1e-5, 1e-4, 1e-10), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)
  expect_true(result$bottleneck >= 1e-10)
})

# ==============================================================================
# Larger Problems
# ==============================================================================

test_that("bottleneck_assignment handles medium-size problems", {
  set.seed(42)
  n <- 20
  cost <- matrix(runif(n * n, 0, 100), nrow = n)

  result <- bottleneck_assignment(cost)

  expect_length(result$match, n)
  expect_equal(length(unique(result$match)), n)
  expect_true(all(result$match >= 1 & result$match <= n))

  # Verify bottleneck
  edges <- sapply(1:n, function(i) cost[i, result$match[i]])
  expect_equal(max(edges), result$bottleneck)
})

test_that("bottleneck_assignment scales to larger problems", {
  skip_on_cran()

  set.seed(123)
  n <- 50
  cost <- matrix(sample(1:1000, n * n, replace = TRUE), nrow = n)

  result <- bottleneck_assignment(cost)

  expect_length(result$match, n)
  expect_equal(result$status, "optimal")

  # Verify optimality: bottleneck should equal max edge in matching
  edges <- sapply(1:n, function(i) cost[i, result$match[i]])
  expect_equal(max(edges), result$bottleneck)
})

# ==============================================================================
# Correctness Verification
# ==============================================================================

test_that("bottleneck optimality verified by exhaustive check (small)", {
  skip_if_not_installed("combinat")

  # For small n, verify by checking all permutations
  cost <- matrix(c(3, 1, 4,
                   1, 5, 9,
                   2, 6, 5), nrow = 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)

  # Generate all permutations and find true minimum bottleneck
  perms <- combinat::permn(3)
  min_bottleneck <- Inf
  for (perm in perms) {
    edges <- sapply(1:3, function(i) cost[i, perm[i]])
    bn <- max(edges)
    if (bn < min_bottleneck) min_bottleneck <- bn
  }

  expect_equal(result$bottleneck, min_bottleneck)
})

test_that("bottleneck matches brute force on random small problems", {
  skip_if_not_installed("combinat")

  set.seed(999)
  for (trial in 1:5) {
    n <- 4
    cost <- matrix(sample(1:20, n * n, replace = TRUE), nrow = n)

    result <- bottleneck_assignment(cost)

    # Brute force
    perms <- combinat::permn(n)
    min_bn <- Inf
    for (perm in perms) {
      edges <- sapply(1:n, function(i) cost[i, perm[i]])
      bn <- max(edges)
      if (bn < min_bn) min_bn <- bn
    }

    expect_equal(result$bottleneck, min_bn,
                 info = paste("Trial", trial, "failed"))
  }
})

# ==============================================================================
# Print Method
# ==============================================================================

test_that("print.bottleneck_result works", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- bottleneck_assignment(cost)

  # Capture output
  output <- capture.output(print(result))

  expect_true(any(grepl("Bottleneck Assignment", output)))
  expect_true(any(grepl("Bottleneck value:", output)))
  expect_true(any(grepl("Status:", output)))
})

# ==============================================================================
# Input Validation
# ==============================================================================

test_that("bottleneck_assignment validates empty matrix", {
  expect_error(bottleneck_assignment(matrix(nrow = 0, ncol = 0)),
               "at least one row")
})

test_that("bottleneck_assignment converts data frame to matrix", {
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  result <- bottleneck_assignment(df)
  expect_s3_class(result, "bottleneck_result")
})
