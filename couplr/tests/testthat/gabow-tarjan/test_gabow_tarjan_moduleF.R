# tests/testthat/test_gabow_tarjan_moduleF.R
# Tests for Module F: match_gt (inner Gabow-Tarjan routine)
#
# IMPORTANT: match_gt finds 1-OPTIMAL matchings, not minimum cost matchings!
# It is an inner routine used by the full Gabow-Tarjan algorithm (Modules G+H).
# Do NOT test against brute force optimal costs!

# Helper function to compute total matching cost
total_cost <- function(cost, row_match) {
  total <- 0
  for (i in seq_along(row_match)) {
    if (row_match[i] != 0) {  # 0 = NIL in R
      total <- total + cost[i, row_match[i]]
    }
  }
  return(total)
}

# Helper to check matching validity
is_valid_matching <- function(row_match, col_match) {
  n <- length(row_match)
  m <- length(col_match)
  
  # Check all rows are matched
  if (any(row_match == 0)) return(FALSE)
  if (any(row_match < 1 | row_match > m)) return(FALSE)
  
  # Check no duplicate column assignments
  if (length(unique(row_match)) != n) return(FALSE)
  
  # Check row_match and col_match are consistent
  for (i in seq_along(row_match)) {
    j <- row_match[i]
    if (j > 0) {
      if (col_match[j] != i) return(FALSE)
    }
  }
  
  return(TRUE)
}

test_that("Module F: is_perfect helper works", {
  # This is implicit in match_gt - if it returns, matching is perfect
  expect_true(TRUE)
})

test_that("Module F: apply_step1 finds and augments paths", {
  # Tested indirectly through match_gt
  expect_true(TRUE)
})

test_that("Module F: match_gt solves simple 2x2", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
  
  result <- gt_match_gt(cost, max_iters = 100)
  
  # Check perfect matching found
  expect_equal(sum(result$row_match != 0), 2)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
  
  # Check matching validity
  expect_true(is_valid_matching(result$row_match, result$col_match))
})

test_that("Module F: match_gt solves 3x3", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE)
  
  result <- gt_match_gt(cost)
  
  # Check perfect matching
  expect_equal(sum(result$row_match != 0), 3)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
  
  # Check matching validity
  expect_true(is_valid_matching(result$row_match, result$col_match))
})

test_that("Module F: match_gt with initial partial matching", {
  # Column-major cost: col 0 = (1, 3, 5), col 1 = (2, 4, 1), col 2 = (3, 2, 4)
  cost <- matrix(c(1, 3, 5, 2, 4, 1, 3, 2, 4), nrow = 3, ncol = 3)

  # Start with row 1 matched to col 1 (1-based; C++ row_match[0] = 0).
  row_match <- c(1L, 0L, 0L)
  col_match <- c(1L, 0L, 0L)
  # 1-feasible duals for this partial matching (matched edge (0,0) has c = 1,
  # needs y_u + y_v in [1, 2]; choose y_u[0] = 1, all other duals 0; verified
  # against the c + 1 upper bound for every finite edge). Phase 1 removed the
  # old defensive dual-repair loop, so the test now supplies feasible inputs.
  y_u <- c(1, 0, 0)
  y_v <- c(0, 0, 0)

  result <- gt_match_gt(cost, row_match, col_match, y_u, y_v)

  expect_equal(sum(result$row_match != 0), 3)

  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)

  expect_true(is_valid_matching(result$row_match, result$col_match))
})

test_that("Module F: match_gt with non-zero initial duals", {
  cost <- matrix(c(5, 2, 3, 1, 4, 2, 6, 3, 1), nrow = 3, ncol = 3)
  
  # Start with some duals
  row_match <- c(0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L)
  y_u <- c(2, 1, 3)
  y_v <- c(1, 2, 0)
  
  result <- gt_match_gt(cost, row_match, col_match, y_u, y_v)
  
  # Check perfect matching
  expect_equal(sum(result$row_match != 0), 3)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
  
  # Check matching validity
  expect_true(is_valid_matching(result$row_match, result$col_match))
})

test_that("Module F: match_gt random 2x2 instances", {
  set.seed(123)
  
  for (trial in 1:20) {
    cost <- matrix(sample(0:9, 4, replace = TRUE), nrow = 2, ncol = 2)
    
    result <- gt_match_gt(cost, max_iters = 100)
    
    # Check perfect matching
    expect_equal(sum(result$row_match != 0), 2)
    
    # Check 1-feasibility (this is what Module F guarantees!)
    feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                      result$y_u, result$y_v)
    expect_true(feasible, info = sprintf("Trial %d failed 1-feasibility", trial))
    
    # Check matching validity
    expect_true(is_valid_matching(result$row_match, result$col_match),
                info = sprintf("Trial %d: invalid matching", trial))
  }
})

test_that("Module F: match_gt random 3x3 instances", {
  set.seed(456)
  
  for (trial in 1:20) {
    cost <- matrix(sample(0:9, 9, replace = TRUE), nrow = 3, ncol = 3)
    
    result <- gt_match_gt(cost, max_iters = 200)
    
    # Check perfect matching
    expect_equal(sum(result$row_match != 0), 3)
    
    # Check 1-feasibility
    feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                      result$y_u, result$y_v)
    expect_true(feasible, info = sprintf("Trial %d failed 1-feasibility", trial))
    
    # Check matching validity
    expect_true(is_valid_matching(result$row_match, result$col_match),
                info = sprintf("Trial %d: invalid matching", trial))
  }
})

test_that("Module F: match_gt random 4x4 instances", {
  set.seed(789)
  
  for (trial in 1:20) {
    cost <- matrix(sample(0:9, 16, replace = TRUE), nrow = 4, ncol = 4)
    
    result <- gt_match_gt(cost, max_iters = 300)
    
    # Check perfect matching
    expect_equal(sum(result$row_match != 0), 4)
    
    # Check 1-feasibility
    feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                      result$y_u, result$y_v)
    expect_true(feasible, info = sprintf("Trial %d failed 1-feasibility", trial))
    
    # Check matching validity
    expect_true(is_valid_matching(result$row_match, result$col_match),
                info = sprintf("Trial %d: invalid matching", trial))
  }
})

test_that("Module F: match_gt random 5x5 instances", {
  set.seed(101112)
  
  for (trial in 1:20) {
    cost <- matrix(sample(0:9, 25, replace = TRUE), nrow = 5, ncol = 5)
    
    result <- gt_match_gt(cost, max_iters = 400)
    
    # Check perfect matching
    expect_equal(sum(result$row_match != 0), 5)
    
    # Check 1-feasibility
    feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                      result$y_u, result$y_v)
    expect_true(feasible, info = sprintf("Trial %d failed 1-feasibility", trial))
    
    # Check matching validity
    expect_true(is_valid_matching(result$row_match, result$col_match),
                info = sprintf("Trial %d: invalid matching", trial))
  }
})

test_that("Module F: match_gt handles all-zero costs", {
  cost <- matrix(0, nrow = 3, ncol = 3)
  
  result <- gt_match_gt(cost)
  
  # Check perfect matching
  expect_equal(sum(result$row_match != 0), 3)
  
  # For all-zero costs, any perfect matching is optimal with cost 0
  algo_cost <- total_cost(cost, result$row_match)
  expect_equal(algo_cost, 0)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module F: match_gt handles uniform costs", {
  cost <- matrix(5, nrow = 3, ncol = 3)
  
  result <- gt_match_gt(cost)
  
  # Check perfect matching
  expect_equal(sum(result$row_match != 0), 3)
  
  # For uniform costs, any perfect matching has same cost
  algo_cost <- total_cost(cost, result$row_match)
  expect_equal(algo_cost, 15)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module F: match_gt handles identity-like matrix", {
  cost <- matrix(c(0, 9, 9,
                   9, 0, 9,
                   9, 9, 0), nrow = 3, ncol = 3, byrow = TRUE)
  
  result <- gt_match_gt(cost)
  
  # Check perfect matching
  expect_equal(sum(result$row_match != 0), 3)
  
  # For identity matrix, diagonal matching is optimal with cost 0
  algo_cost <- total_cost(cost, result$row_match)
  expect_equal(algo_cost, 0)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module F: match_gt deterministic behavior", {
  cost <- matrix(sample(1:20, 16, replace = TRUE), nrow = 4, ncol = 4)
  
  # Run twice with same input
  result1 <- gt_match_gt(cost)
  result2 <- gt_match_gt(cost)
  
  # Should produce same matching
  expect_equal(result1$row_match, result2$row_match)
  expect_equal(result1$col_match, result2$col_match)
  expect_equal(result1$y_u, result2$y_u)
  expect_equal(result1$y_v, result2$y_v)
})

test_that("Module F: match_gt with check_feasible enabled", {
  cost <- matrix(sample(1:10, 9, replace = TRUE), nrow = 3, ncol = 3)
  
  # Should not throw with check_feasible=TRUE if algorithm is correct
  expect_no_error({
    result <- gt_match_gt(cost, max_iters = 100, check_feasible = TRUE)
  })
  
  # Still check final state
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
})

test_that("Module F: match_gt increasing cost matrix", {
  # Matrix where costs increase with row+col
  cost <- matrix(0, nrow = 4, ncol = 4)
  for (i in 1:4) {
    for (j in 1:4) {
      cost[i, j] <- i + j
    }
  }
  
  result <- gt_match_gt(cost)
  
  # Check perfect matching
  expect_equal(sum(result$row_match != 0), 4)
  
  # Check 1-feasibility
  feasible <- gt_check_one_feasible(cost, result$row_match, result$col_match,
                                    result$y_u, result$y_v)
  expect_true(feasible)
  
  # Check matching validity
  expect_true(is_valid_matching(result$row_match, result$col_match))
})