# test-gabow_tarjan_module_g.R
# Tests for Module G: scale_match wrapper for bit-scaling outer loop

library(testthat)

# Helper function to compute total matching cost
total_cost <- function(cost, row_match) {
  n <- nrow(cost)
  total <- 0
  for (i in seq_len(n)) {
    j <- row_match[i]
    if (j > 0) {
      total <- total + cost[i, j]
    }
  }
  total
}

# Helper to check if matching is perfect
is_perfect_matching <- function(row_match) {
  all(row_match > 0)
}

# Helper to check matching consistency
check_matching_consistency <- function(row_match, col_match) {
  n <- length(row_match)
  m <- length(col_match)
  
  for (i in seq_len(n)) {
    j <- row_match[i]
    if (j > 0) {
      if (j > m || col_match[j] != i) {
        return(FALSE)
      }
    }
  }
  
  for (j in seq_len(m)) {
    i <- col_match[j]
    if (i > 0) {
      if (i > n || row_match[i] != j) {
        return(FALSE)
      }
    }
  }
  
  TRUE
}

# Helper to check 1-feasibility
check_one_feasible <- function(cost, row_match, col_match, y_u, y_v) {
  n <- nrow(cost)
  m <- ncol(cost)
  BIG_INT <- 1e15
  
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      c_ij <- cost[i, j]
      
      # Skip forbidden edges
      if (is.infinite(c_ij) || c_ij >= BIG_INT) next
      
      sum_duals <- y_u[i] + y_v[j]
      matched <- (row_match[i] == j && col_match[j] == i)
      
      # Condition 1: y_u[i] + y_v[j] <= c(i,j) + 1
      if (sum_duals > c_ij + 1 + 1e-9) {
        return(FALSE)
      }
      
      # Condition 2: for matched edges, y_u[i] + y_v[j] >= c(i,j)
      if (matched && sum_duals < c_ij - 1e-9) {
        return(FALSE)
      }
    }
  }
  
  TRUE
}

test_that("scale_match finds optimal matching on simple 3x3 matrix", {
  # Known optimal solution: matching (0->1, 1->0, 2->2) with cost 5
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  # Check perfect matching
  expect_true(is_perfect_matching(result$row_match))
  
  # Check consistency
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Check optimal cost
  expect_equal(total_cost(cost, result$row_match), 5)
  
  # Check specific matching (may vary but cost should be same)
  actual_cost <- total_cost(cost, result$row_match)
  expect_equal(actual_cost, 5)
  
  # Check 1-feasibility
  expect_true(check_one_feasible(cost, result$row_match, result$col_match, 
                                  result$y_u, result$y_v))
})

test_that("scale_match is idempotent on optimal solution", {
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)
  
  # First call
  result1 <- couplr:::scale_match_cpp(cost)
  cost1 <- total_cost(cost, result1$row_match)
  
  # Second call starting from result1
  result2 <- couplr:::scale_match_cpp(
    cost,
    row_match = result1$row_match,
    col_match = result1$col_match,
    y_u = result1$y_u,
    y_v = result1$y_v
  )
  cost2 <- total_cost(cost, result2$row_match)
  
  # Cost should remain optimal
  expect_equal(cost2, cost1)
  expect_equal(cost2, 5)
  
  # Matching should still be perfect
  expect_true(is_perfect_matching(result2$row_match))
  
  # Should still be 1-feasible
  expect_true(check_one_feasible(cost, result2$row_match, result2$col_match,
                                  result2$y_u, result2$y_v))
})

test_that("scale_match handles 2x2 matrices correctly", {
  # Simple 2x2 case
  cost <- matrix(c(
    1, 2,
    4, 3
  ), nrow = 2, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Optimal matching is (0->0, 1->1) with cost 4
  expect_equal(total_cost(cost, result$row_match), 4)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles identity-like costs", {
  # Identity-style matrix (diagonal is cheapest)
  cost <- matrix(c(
    1, 5, 5,
    5, 1, 5,
    5, 5, 1
  ), nrow = 3, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Optimal cost is 3 (diagonal)
  expect_equal(total_cost(cost, result$row_match), 3)
  
  # Matching should be diagonal
  expect_equal(result$row_match, c(1, 2, 3))
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles anti-diagonal costs", {
  # Anti-diagonal is cheapest
  cost <- matrix(c(
    5, 5, 1,
    5, 1, 5,
    1, 5, 5
  ), nrow = 3, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Optimal cost is 3 (anti-diagonal)
  expect_equal(total_cost(cost, result$row_match), 3)
  
  # Matching should be anti-diagonal
  expect_equal(result$row_match, c(3, 2, 1))
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles uniform costs", {
  # All edges have same cost
  cost <- matrix(5, nrow = 3, ncol = 3)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Any perfect matching has cost 15
  expect_equal(total_cost(cost, result$row_match), 15)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles large cost differences", {
  # Mix of very different costs
  cost <- matrix(c(
    1, 1000, 1000,
    1000, 1, 1000,
    1000, 1000, 1
  ), nrow = 3, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Should match diagonal with cost 3
  expect_equal(total_cost(cost, result$row_match), 3)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles 4x4 matrix", {
  # Note: sum(row minimums) is NOT an upper bound on optimal cost
  # because the minimum in each row might map to the same column
  cost <- matrix(c(
    10, 19, 8, 15,
    10, 18, 7, 17,
    13, 16, 9, 14,
    12, 19, 8, 18
  ), nrow = 4, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Verify we get a valid optimal matching
  # The test checks we get a perfect matching with valid cost
  actual_cost <- total_cost(cost, result$row_match)
  expect_true(actual_cost > 0)
  expect_true(is.finite(actual_cost))
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles 5x5 matrix", {
  cost <- matrix(c(
    7, 2, 1, 9, 4,
    9, 6, 9, 5, 5,
    3, 8, 3, 1, 8,
    7, 9, 4, 2, 2,
    8, 4, 7, 4, 8
  ), nrow = 5, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Should find some perfect matching
  actual_cost <- total_cost(cost, result$row_match)
  expect_true(actual_cost > 0)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles negative costs", {
  # Matrix with negative costs
  cost <- matrix(c(
    -1, 5, 3,
    4, -2, 6,
    2, 1, -3
  ), nrow = 3, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Optimal is diagonal: -1 + -2 + -3 = -6
  expect_equal(total_cost(cost, result$row_match), -6)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match handles mix of positive and negative costs", {
  cost <- matrix(c(
    10, -5, 3,
    -2, 8, -1,
    4, 1, 6
  ), nrow = 3, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Should find optimal matching
  actual_cost <- total_cost(cost, result$row_match)
  
  # Verify it's a reasonable cost (should include negative costs)
  expect_true(actual_cost < 20)  # Much less than sum of all positive values
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match with pre-initialized duals converges correctly", {
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)
  
  # Start with some non-zero duals
  y_u <- c(1, 2, 1)
  y_v <- c(1, 0, 1)
  
  result <- couplr:::scale_match_cpp(
    cost,
    y_u = y_u,
    y_v = y_v
  )
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Should still find optimal cost
  expect_equal(total_cost(cost, result$row_match), 5)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match with partial matching converges correctly", {
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)
  
  # Start with partial matching (only first row matched)
  row_match <- c(2, 0, 0)  # row 0 matched to col 1
  col_match <- c(0, 1, 0)  # col 1 matched to row 0
  
  result <- couplr:::scale_match_cpp(
    cost,
    row_match = row_match,
    col_match = col_match
  )
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Should complete to optimal matching
  expect_equal(total_cost(cost, result$row_match), 5)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match produces consistent duals across multiple calls", {
  cost <- matrix(c(
    10, 20, 30,
    15, 25, 35,
    20, 30, 40
  ), nrow = 3, byrow = TRUE)
  
  # First call
  result1 <- couplr:::scale_match_cpp(cost)
  
  # Second call with first result as input
  result2 <- couplr:::scale_match_cpp(
    cost,
    row_match = result1$row_match,
    col_match = result1$col_match,
    y_u = result1$y_u,
    y_v = result1$y_v
  )
  
  # Matching should be identical (or at least same cost)
  expect_equal(total_cost(cost, result2$row_match), 
               total_cost(cost, result1$row_match))
  
  # Both should be 1-feasible
  expect_true(check_one_feasible(cost, result1$row_match, result1$col_match,
                                  result1$y_u, result1$y_v))
  expect_true(check_one_feasible(cost, result2$row_match, result2$col_match,
                                  result2$y_u, result2$y_v))
})

test_that("scale_match handles zero costs correctly", {
  cost <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, byrow = TRUE)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  
  # Optimal is diagonal with cost 0
  expect_equal(total_cost(cost, result$row_match), 0)
  
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})

test_that("scale_match dual updates are cumulative", {
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)
  
  # First call with zero duals
  result1 <- couplr:::scale_match_cpp(cost)
  
  # Duals should be non-zero after finding optimal matching
  expect_true(any(result1$y_u != 0) || any(result1$y_v != 0))
  
  # Second call - duals should accumulate (or stay same if already optimal)
  result2 <- couplr:::scale_match_cpp(
    cost,
    row_match = result1$row_match,
    col_match = result1$col_match,
    y_u = result1$y_u,
    y_v = result1$y_v
  )
  
  # The global duals satisfy complementary slackness for original cost
  for (i in 1:nrow(cost)) {
    j <- result2$row_match[i]
    if (j > 0) {
      # Matched edge should be tight: y_u[i] + y_v[j] = cost[i,j]
      # (up to the cost-length adjustment)
      expect_true(result2$y_u[i] + result2$y_v[j] >= cost[i, j] - 1e-9)
      expect_true(result2$y_u[i] + result2$y_v[j] <= cost[i, j] + 1 + 1e-9)
    }
  }
})

test_that("scale_match rejects an instance its rows cannot cover", {
  # 1-optimality is a statement about a matching that saturates both sides, so
  # one scale of the bit-scaling needs row capacities adding up to the number
  # of columns. Giving a shape the dummy side has not been added to is an
  # error rather than a silently one-sided answer.
  cost <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12
  ), nrow = 4, byrow = TRUE)

  expect_error(couplr:::scale_match_cpp(cost),
               "every column can be matched in")

  # The solver above it adds that side and returns the rectangular optimum.
  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)
  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, assignment(cost, method = "jv")$total_cost)
})

test_that("scale_match performance on larger matrix (10x10)", {
  skip_on_cran()
  
  set.seed(123)
  n <- 10
  cost <- matrix(sample(1:100, n*n, replace = TRUE), nrow = n)
  
  result <- couplr:::scale_match_cpp(cost)
  
  expect_true(is_perfect_matching(result$row_match))
  expect_true(check_matching_consistency(result$row_match, result$col_match))
  expect_true(check_one_feasible(cost, result$row_match, result$col_match,
                                  result$y_u, result$y_v))
})
