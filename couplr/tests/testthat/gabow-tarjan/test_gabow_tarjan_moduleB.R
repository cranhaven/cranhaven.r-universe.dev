# tests/testthat/test_gabow_tarjan_moduleB.R

test_that("Module B: basic equality graph with all eligible edges", {
  # 2x2 all-zero costs, duals y_u = 1, y_v = 0
  # For unmatched edges: cl = 0 + 1 = 1, and y_u + y_v = 1
  # So all edges are eligible
  cost <- matrix(c(
    0, 0,
    0, 0
  ), nrow = 2, byrow = TRUE)
  
  y_u <- c(1, 1)
  y_v <- c(0, 0)
  row_match <- c(0L, 0L)  # NIL represented as 0
  
  eq_graph <- gt_build_equality_graph(cost, row_match, y_u, y_v)
  
  # Both rows should have both columns eligible
  expect_equal(length(eq_graph), 2)
  expect_equal(sort(eq_graph[[1]]), c(1L, 2L))
  expect_equal(sort(eq_graph[[2]]), c(1L, 2L))
})

test_that("Module B: equality graph with forbidden edges", {
  BIG_INT <- 1e15
  
  # One forbidden edge in row 0, col 1
  cost <- matrix(c(
    0, BIG_INT,
    0, 0
  ), nrow = 2, byrow = TRUE)
  
  y_u <- c(1, 1)
  y_v <- c(0, 0)
  row_match <- c(0L, 0L)  # NIL
  
  eq_graph <- gt_build_equality_graph(cost, row_match, y_u, y_v)
  
  # Row 0: only col 0 is finite and eligible
  expect_equal(eq_graph[[1]], 1L)
  
  # Row 1: both edges finite and eligible
  expect_equal(sort(eq_graph[[2]]), c(1L, 2L))
})

test_that("Module B: equality graph with matched edges", {
  # 2x2 with matching on diagonal
  cost <- matrix(c(
    1, 4,
    3, 2
  ), nrow = 2, byrow = TRUE)
  
  # Matching: row 0 -> col 0, row 1 -> col 1
  row_match <- c(1L, 2L)
  
  # Set duals so matched edges are eligible
  # For matched edge (0,0): cl = 1, need y_u[0] + y_v[0] = 1
  # For matched edge (1,1): cl = 2, need y_u[1] + y_v[1] = 2
  y_u <- c(1, 1)
  y_v <- c(0, 1)
  
  eq_graph <- gt_build_equality_graph(cost, row_match, y_u, y_v)
  
  # Row 0: edge (0,0) is matched with cl=1, y_sum=1 -> eligible
  #        edge (0,1) is unmatched with cl=5, y_sum=2 -> not eligible
  expect_equal(eq_graph[[1]], 1L)
  
  # Row 1: edge (1,0) is unmatched with cl=4, y_sum=1 -> not eligible
  #        edge (1,1) is matched with cl=2, y_sum=2 -> eligible
  expect_equal(eq_graph[[2]], 2L)
})

test_that("Module B: equality graph with no eligible edges", {
  cost <- matrix(c(
    5, 10,
    15, 20
  ), nrow = 2, byrow = TRUE)
  
  # Set duals so no edges are eligible
  y_u <- c(0, 0)
  y_v <- c(0, 0)
  row_match <- c(0L, 0L)  # NIL
  
  eq_graph <- gt_build_equality_graph(cost, row_match, y_u, y_v)
  
  # No edges should be eligible (cl=6,11,16,21 but y_sum=0)
  expect_equal(length(eq_graph[[1]]), 0)
  expect_equal(length(eq_graph[[2]]), 0)
})

test_that("Module B: equality graph with mixed eligible/ineligible", {
  # 3x3 with selective eligibility
  cost <- matrix(c(
    0, 1, 2,
    1, 0, 1,
    2, 1, 0
  ), nrow = 3, byrow = TRUE)
  
  row_match <- c(0L, 0L, 0L)  # All unmatched
  
  # Carefully chosen duals to make specific edges eligible
  # For unmatched edges, cl = c + 1
  y_u <- c(1, 0, 0)
  y_v <- c(0, 1, 1)
  
  eq_graph <- gt_build_equality_graph(cost, row_match, y_u, y_v)
  
  # Row 0: y_u[0]=1
  #   (0,0): cost=0, cl=1, y_sum=1+0=1 -> eligible
  #   (0,1): cost=1, cl=2, y_sum=1+1=2 -> eligible
  #   (0,2): cost=2, cl=3, y_sum=1+1=2 -> not eligible
  expect_equal(sort(eq_graph[[1]]), c(1L, 2L))
  
  # Row 1: y_u[1]=0
  #   (1,0): cost=1, cl=2, y_sum=0+0=0 -> not eligible
  #   (1,1): cost=0, cl=1, y_sum=0+1=1 -> eligible
  #   (1,2): cost=1, cl=2, y_sum=0+1=1 -> not eligible
  expect_equal(eq_graph[[2]], 2L)
  
  # Row 2: y_u[2]=0
  #   (2,0): cost=2, cl=3, y_sum=0+0=0 -> not eligible
  #   (2,1): cost=1, cl=2, y_sum=0+1=1 -> not eligible
  #   (2,2): cost=0, cl=1, y_sum=0+1=1 -> eligible
  expect_equal(eq_graph[[3]], 3L)
})

test_that("Module B: equality graph returns proper list structure", {
  cost <- matrix(c(
    0, 0,
    0, 0
  ), nrow = 2, byrow = TRUE)
  
  y_u <- c(1, 1)
  y_v <- c(0, 0)
  row_match <- c(0L, 0L)
  
  eq_graph <- gt_build_equality_graph(cost, row_match, y_u, y_v)
  
  # Should return a list
  expect_type(eq_graph, "list")
  expect_equal(length(eq_graph), 2)
  
  # Each element should be an integer vector
  expect_type(eq_graph[[1]], "integer")
  expect_type(eq_graph[[2]], "integer")
})

test_that("Module B: equality graph with empty matrix", {
  cost <- matrix(numeric(0), nrow = 0, ncol = 0)
  row_match <- integer(0)
  y_u <- numeric(0)
  y_v <- numeric(0)
  
  eq_graph <- gt_build_equality_graph(cost, row_match, y_u, y_v)
  
  expect_type(eq_graph, "list")
  expect_equal(length(eq_graph), 0)
})
