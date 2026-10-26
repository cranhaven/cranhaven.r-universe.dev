# tests/testthat/test_gabow_tarjan_moduleA.R

test_that("Module A: cost_length works correctly", {
  # Edge in matching
  expect_equal(gt_cost_length(5L, TRUE), 5L)
  
  # Edge not in matching
  expect_equal(gt_cost_length(5L, FALSE), 6L)
  
  # Negative costs
  expect_equal(gt_cost_length(-1L, TRUE), -1L)
  expect_equal(gt_cost_length(-1L, FALSE), 0L)
  
  # Zero cost
  expect_equal(gt_cost_length(0L, TRUE), 0L)
  expect_equal(gt_cost_length(0L, FALSE), 1L)
})

test_that("Module A: is_eligible works correctly", {
  c <- 5L
  
  # Edge in matching: cl = 5, should be eligible when yu + yv = 5
  expect_true(gt_is_eligible(c, TRUE, 3L, 2L))
  expect_false(gt_is_eligible(c, TRUE, 3L, 1L))
  
  # Edge not in matching: cl = 6, should be eligible when yu + yv = 6
  expect_true(gt_is_eligible(c, FALSE, 3L, 3L))
  expect_false(gt_is_eligible(c, FALSE, 3L, 2L))
  
  # Zero cost edge in matching
  expect_true(gt_is_eligible(0L, TRUE, 0L, 0L))
  expect_true(gt_is_eligible(0L, TRUE, -1L, 1L))
})

test_that("Module A: check_one_feasible with valid 1-feasible matching", {
  # 2x2 example with perfect matching on diagonal
  cost <- matrix(c(
    1, 4,
    3, 2
  ), nrow = 2, byrow = TRUE)
  
  # Matching: 0->0, 1->1 (R 1-based: 1->1, 2->2)
  row_match <- c(1L, 2L)
  col_match <- c(1L, 2L)
  
  # Duals chosen so:
  # (0,0) matched: y=1+0=1 == c(0,0)=1  ✓
  # (1,1) matched: y=1+1=2 == c(1,1)=2  ✓
  # (0,1) unmatched: y=1+1=2 <= 4+1=5   ✓
  # (1,0) unmatched: y=1+0=1 <= 3+1=4   ✓
  y_u <- c(1, 1)
  y_v <- c(0, 1)
  
  expect_true(gt_check_one_feasible(cost, row_match, col_match, y_u, y_v))
})

test_that("Module A: check_one_feasible detects violation on matched edge", {
  cost <- matrix(c(
    1, 4,
    3, 2
  ), nrow = 2, byrow = TRUE)
  
  row_match <- c(1L, 2L)
  col_match <- c(1L, 2L)
  
  # Violation: matched edge (0,0) with y sum too small
  # (0,0) matched: 0+0=0 < 1 ⇒ violation
  y_u <- c(0, 1)
  y_v <- c(0, 1)
  
  expect_false(gt_check_one_feasible(cost, row_match, col_match, y_u, y_v))
})

test_that("Module A: check_one_feasible detects violation on unmatched edge", {
  cost <- matrix(c(
    1, 4,
    3, 2
  ), nrow = 2, byrow = TRUE)
  
  row_match <- c(1L, 2L)
  col_match <- c(1L, 2L)
  
  # Violation: unmatched edge (0,1) with y sum > c+1
  # (0,1) unmatched: 10+1=11 > 4+1=5 ⇒ violation
  y_u <- c(10, 1)
  y_v <- c(0, 1)
  
  expect_false(gt_check_one_feasible(cost, row_match, col_match, y_u, y_v))
})

test_that("Module A: check_one_feasible handles empty matching", {
  cost <- matrix(c(
    1, 2,
    3, 4
  ), nrow = 2, byrow = TRUE)
  
  # Empty matching (all NIL, represented as 0 in R)
  row_match <- c(0L, 0L)
  col_match <- c(0L, 0L)
  
  # All zeros for duals
  y_u <- c(0, 0)
  y_v <- c(0, 0)
  
  # All edges unmatched: y_u[i] + y_v[j] <= c(i,j) + 1
  # (0,0): 0+0=0 <= 1+1=2 ✓
  # (0,1): 0+0=0 <= 2+1=3 ✓
  # (1,0): 0+0=0 <= 3+1=4 ✓
  # (1,1): 0+0=0 <= 4+1=5 ✓
  expect_true(gt_check_one_feasible(cost, row_match, col_match, y_u, y_v))
})

test_that("Module A: check_one_feasible handles BIG_INT (forbidden edges)", {
  BIG_INT <- 1e15
  
  cost <- matrix(c(
    1, BIG_INT,
    BIG_INT, 2
  ), nrow = 2, byrow = TRUE)
  
  row_match <- c(1L, 2L)
  col_match <- c(1L, 2L)
  
  # Set duals that would violate if BIG_INT edges weren't ignored
  y_u <- c(1, 1)
  y_v <- c(0, 1)
  
  # Should still be valid because forbidden edges are skipped
  expect_true(gt_check_one_feasible(cost, row_match, col_match, y_u, y_v))
})