# tests/testthat/test_gabow_tarjan_moduleE.R

# build_cl_matrix was removed: cost-length is now computed inline inside
# hungarian_search_cl. cost_length() / is_eligible() in Module A still exist
# and are exercised in test_gabow_tarjan_moduleA.R; the cl identity for
# matched/unmatched/forbidden edges is implicit in the Hungarian-step tests
# below (forbidden edges never appear in a final matching; matched edges
# carry the bare cost; unmatched edges contribute cost + 1).

test_that("Module E: hungarian_step finds augmenting path for 2x2", {
  # Simple 2x2 with zero costs, empty matching, zero duals
  cost <- matrix(c(0L, 0L, 0L, 0L), nrow = 2, ncol = 2)
  row_match <- c(0L, 0L)  # NIL
  col_match <- c(0L, 0L)  # NIL
  y_u <- c(0L, 0L)
  y_v <- c(0L, 0L)
  
  result <- gt_hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v)
  
  # Should find an augmenting path
  expect_true(result$found)
  
  # Matching should increase by 1
  matched_count <- sum(result$row_match != 0)
  expect_equal(matched_count, 1)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match, 
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module E: hungarian_step handles non-zero costs", {
  # 2x2 with non-zero costs
  cost <- matrix(c(2L, 5L, 3L, 1L), nrow = 2, ncol = 2)
  row_match <- c(0L, 0L)
  col_match <- c(0L, 0L)
  y_u <- c(0L, 0L)
  y_v <- c(0L, 0L)
  
  result <- gt_hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v)
  
  expect_true(result$found)
  expect_equal(sum(result$row_match != 0), 1)
  
  # Verify 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module E: repeated steps reach perfect matching for 2x2", {
  cost <- matrix(c(4L, 2L, 3L, 5L), nrow = 2, ncol = 2)
  row_match <- c(0L, 0L)
  col_match <- c(0L, 0L)
  y_u <- c(0L, 0L)
  y_v <- c(0L, 0L)
  
  # First step
  result1 <- gt_hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v)
  expect_true(result1$found)
  expect_equal(sum(result1$row_match != 0), 1)
  
  # Second step should complete the matching
  result2 <- gt_hungarian_step_one_feasible(cost, result1$row_match, result1$col_match,
                                            result1$y_u, result1$y_v)
  expect_true(result2$found)
  expect_equal(sum(result2$row_match != 0), 2)
  
  # Verify final state is 1-feasible
  feasible <- gt_check_one_feasible(cost, result2$row_match, result2$col_match,
                                    result2$y_u, result2$y_v)
  expect_true(feasible)
})

test_that("Module E: returns false when matching already perfect", {
  cost <- matrix(c(1L, 2L, 3L, 4L), nrow = 2, ncol = 2)
  row_match <- c(1L, 2L)  # Perfect matching
  col_match <- c(1L, 2L)
  y_u <- c(1L, 1L)
  y_v <- c(0L, 3L)
  
  result <- gt_hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v)
  
  # Should return false (no free row)
  expect_false(result$found)
})

test_that("Module E: handles 3x3 matrix correctly", {
  cost <- matrix(c(
    5L, 9L, 3L,
    8L, 7L, 8L,
    4L, 2L, 5L
  ), nrow = 3, ncol = 3)
  
  row_match <- c(0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L)
  y_u <- c(0L, 0L, 0L)
  y_v <- c(0L, 0L, 0L)
  
  # Run three steps to get perfect matching
  result <- list(row_match = row_match, col_match = col_match, 
                 y_u = y_u, y_v = y_v, found = TRUE)
  
  for (i in 1:3) {
    result <- gt_hungarian_step_one_feasible(cost, result$row_match, result$col_match,
                                             result$y_u, result$y_v)
    expect_true(result$found)
    expect_equal(sum(result$row_match != 0), i)
  }
  
  # Final matching should be perfect and 1-feasible
  expect_equal(sum(result$row_match != 0), 3)
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module E: handles forbidden edges correctly", {
  BIG_INT <- 1000000000000000
  
  cost <- matrix(c(
    1L, BIG_INT, 4L,
    BIG_INT, 2L, 5L,
    3L, 6L, BIG_INT
  ), nrow = 3, ncol = 3)
  
  row_match <- c(0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L)
  y_u <- c(0L, 0L, 0L)
  y_v <- c(0L, 0L, 0L)
  
  # Run steps
  result <- list(row_match = row_match, col_match = col_match,
                 y_u = y_u, y_v = y_v, found = TRUE)
  
  for (i in 1:3) {
    result <- gt_hungarian_step_one_feasible(cost, result$row_match, result$col_match,
                                             result$y_u, result$y_v)
    expect_true(result$found)
  }
  
  # Verify no forbidden edges are used
  for (i in 1:3) {
    if (result$row_match[i] != 0) {
      j <- result$row_match[i]
      expect_true(cost[i, j] < BIG_INT)
    }
  }
  
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module E: dual adjustments maintain feasibility", {
  cost <- matrix(c(
    10L, 15L, 20L,
    12L, 14L, 13L,
    11L, 16L, 18L
  ), nrow = 3, ncol = 3)
  
  row_match <- c(0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L)
  y_u <- c(0L, 0L, 0L)
  y_v <- c(0L, 0L, 0L)
  
  result <- list(row_match = row_match, col_match = col_match,
                 y_u = y_u, y_v = y_v, found = TRUE)
  
  # Each step should maintain 1-feasibility
  for (i in 1:3) {
    result <- gt_hungarian_step_one_feasible(cost, result$row_match, result$col_match,
                                             result$y_u, result$y_v)
    expect_true(result$found)
    
    feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                      result$y_u, result$y_v)
    expect_true(feasible)
  }
})

test_that("Module E: finds optimal matching for small instance", {
  # Cost matrix where optimal matching is known
  cost <- matrix(c(
    3L, 1L, 4L,
    2L, 5L, 3L,
    4L, 2L, 1L
  ), nrow = 3, ncol = 3)
  
  # Optimal: (0,1)=1, (1,0)=2, (2,2)=1, total=4
  
  row_match <- c(0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L)
  y_u <- c(0L, 0L, 0L)
  y_v <- c(0L, 0L, 0L)
  
  result <- list(row_match = row_match, col_match = col_match,
                 y_u = y_u, y_v = y_v, found = TRUE)
  
  for (i in 1:3) {
    result <- gt_hungarian_step_one_feasible(cost, result$row_match, result$col_match,
                                             result$y_u, result$y_v)
  }
  
  # Calculate matching cost
  total_cost <- 0
  for (i in 1:3) {
    if (result$row_match[i] != 0) {
      j <- result$row_match[i]
      total_cost <- total_cost + cost[i, j]
    }
  }
  
  # Should find optimal cost of 4
  expect_equal(total_cost, 4)
})

test_that("Module E: handles negative costs", {
  # Note: Gabow-Tarjan assumes costs >= -1 for 1-feasibility to work
  # with zero initial duals. For costs < -1, we need to adjust duals.
  cost <- matrix(c(
    0L, 2L, 3L,
    1L, -1L, 2L,
    2L, 1L, 0L
  ), nrow = 3, ncol = 3)
  
  row_match <- c(0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L)
  y_u <- c(0L, 0L, 0L)
  y_v <- c(0L, 0L, 0L)
  
  result <- list(row_match = row_match, col_match = col_match,
                 y_u = y_u, y_v = y_v, found = TRUE)
  
  for (i in 1:3) {
    result <- gt_hungarian_step_one_feasible(cost, result$row_match, result$col_match,
                                             result$y_u, result$y_v)
    expect_true(result$found)
    
    # Should maintain 1-feasibility even with negative costs
    feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                      result$y_u, result$y_v)
    expect_true(feasible)
  }
  
  expect_equal(sum(result$row_match != 0), 3)
})

test_that("Module E: empty matrix returns false", {
  cost <- matrix(integer(0), nrow = 0, ncol = 0)
  row_match <- integer(0)
  col_match <- integer(0)
  y_u <- numeric(0)
  y_v <- numeric(0)
  
  result <- gt_hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v)
  
  expect_false(result$found)
})

test_that("Module E: single element matrix", {
  cost <- matrix(c(5L), nrow = 1, ncol = 1)
  row_match <- c(0L)
  col_match <- c(0L)
  y_u <- c(0L)
  y_v <- c(0L)
  
  result <- gt_hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v)
  
  expect_true(result$found)
  expect_equal(result$row_match[1], 1L)
  expect_equal(result$col_match[1], 1L)
  
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module E: comparison with brute force on small random instances", {
  set.seed(42)
  
  for (trial in 1:5) {
    # Generate random 3x3 cost matrix
    cost <- matrix(sample(-5:10, 9, replace = TRUE), nrow = 3, ncol = 3)
    
    row_match <- c(0L, 0L, 0L)
    col_match <- c(0L, 0L, 0L)
    y_u <- c(0L, 0L, 0L)
    y_v <- c(0L, 0L, 0L)
    
    result <- list(row_match = row_match, col_match = col_match,
                   y_u = y_u, y_v = y_v, found = TRUE)
    
    # Run algorithm
    for (i in 1:3) {
      result <- gt_hungarian_step_one_feasible(cost, result$row_match, result$col_match,
                                               result$y_u, result$y_v)
      if (!result$found) break
    }
    
    # Should find a perfect matching
    expect_equal(sum(result$row_match != 0), 3)
    
    # Should be 1-feasible
    feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                      result$y_u, result$y_v)
    expect_true(feasible)
    
    # Calculate cost
    algo_cost <- 0
    for (i in 1:3) {
      if (result$row_match[i] != 0) {
        algo_cost <- algo_cost + cost[i, result$row_match[i]]
      }
    }
    
    # Brute force check (all permutations)
    min_cost <- Inf
    for (perm in list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))) {
      perm_cost <- sum(cost[cbind(1:3, perm)])
      min_cost <- min(min_cost, perm_cost)
    }
    
    # Algorithm should find optimal or near-optimal (within 1-optimality)
    expect_true(algo_cost <= min_cost + 3)  # Within 3n tolerance from Lemma 2.1
  }
})
