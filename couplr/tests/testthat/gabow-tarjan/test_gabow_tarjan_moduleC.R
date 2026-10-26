# tests/testthat/test_gabow_tarjan_moduleC.R

test_that("Module C: simple augment on empty matching", {
  # 2x2, empty matching, augment with single edge (0,1)
  row_match <- c(0L, 0L)  # NIL
  col_match <- c(0L, 0L)  # NIL
  
  # Edge (0,1) in 0-based becomes (1,2) in R 1-based
  edges <- matrix(c(1L, 2L), nrow = 1, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  # After augmenting, row 0 should match col 1
  expect_equal(result$row_match, c(2L, 0L))
  expect_equal(result$col_match, c(0L, 1L))
  
  # Matching size should be 1
  expect_equal(sum(result$row_match > 0), 1)
})

test_that("Module C: augment with longer alternating path", {
  # 3x3, initial matching: {(0,0), (1,1)}
  row_match <- c(1L, 2L, 0L)  # 0->0, 1->1, 2->NIL
  col_match <- c(1L, 2L, 0L)  # 0->0, 1->1, 2->NIL
  
  # Current C++ path representation stores the new selected edges.
  # This reassigns row 2 to col 1 and row 1 to free col 2.
  edges <- matrix(c(
    3L, 2L,
    2L, 3L
  ), nrow = 2, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  # Final matching should be: {(0,0), (2,1), (1,2)}
  # In R 1-based: {(1,1), (3,2), (2,3)}
  expect_equal(result$row_match, c(1L, 3L, 2L))
  expect_equal(result$col_match, c(1L, 3L, 2L))
  
  # Matching size should be 3 (perfect)
  expect_equal(sum(result$row_match > 0), 3)
})

test_that("Module C: augment preserves matching property", {
  # 4x4, partial matching: {(0,0), (1,1)}
  row_match <- c(1L, 2L, 0L, 0L)
  col_match <- c(1L, 2L, 0L, 0L)
  
  # Proper augmenting path from free row 2 to free col 2:
  # Just a single edge (2,2) since both endpoints are free
  # In R 1-based: (3,3)
  edges <- matrix(c(3L, 3L), nrow = 1, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  # Result should be: {(0,0), (1,1), (2,2)}
  # In R: {(1,1), (2,2), (3,3)}
  
  # Check matching property: each row matches at most one col
  matched_cols <- result$row_match[result$row_match > 0]
  expect_equal(length(matched_cols), length(unique(matched_cols)))
  
  # Check inverse property: col_match[j] = i iff row_match[i] = j
  for (i in seq_along(result$row_match)) {
    j <- result$row_match[i]
    if (j > 0) {
      expect_equal(result$col_match[j], i)
    }
  }
  
  for (j in seq_along(result$col_match)) {
    i <- result$col_match[j]
    if (i > 0) {
      expect_equal(result$row_match[i], j)
    }
  }
})

test_that("Module C: symmetric difference increases matching size by 1", {
  # Start with matching of size 2
  row_match <- c(2L, 3L, 0L, 0L)
  col_match <- c(0L, 1L, 2L, 0L)
  
  initial_size <- sum(row_match > 0)
  expect_equal(initial_size, 2)
  
  # True augmenting path should increase size by 1
  # Path: (2,0), (2,3)
  # In R: (3,1), (3,4)
  edges <- matrix(c(
    3L, 1L,
    3L, 4L
  ), nrow = 2, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  final_size <- sum(result$row_match > 0)
  expect_equal(final_size, initial_size + 1)
})

test_that("Module C: augment with single-edge path", {
  # 2x2, matching has one edge
  row_match <- c(1L, 0L)
  col_match <- c(1L, 0L)
  
  # Augment with (1,1) - in R: (2,2)
  edges <- matrix(c(2L, 2L), nrow = 1, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  # Should now have perfect matching
  expect_equal(result$row_match, c(1L, 2L))
  expect_equal(result$col_match, c(1L, 2L))
  expect_equal(sum(result$row_match > 0), 2)
})

test_that("Module C: augment handles edge removal and addition correctly", {
  # 3x3, matching: {(0,1), (1,2)}
  row_match <- c(2L, 3L, 0L)
  col_match <- c(0L, 1L, 2L)
  
  # New selected edges that reassign rows to the diagonal.
  edges <- matrix(c(
    3L, 3L,
    2L, 2L,
    1L, 1L
  ), nrow = 3, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  # Final matching should be: {(0,0), (1,1), (2,2)}
  expect_equal(result$row_match, c(1L, 2L, 3L))
  expect_equal(result$col_match, c(1L, 2L, 3L))
  expect_equal(sum(result$row_match > 0), 3)
})

test_that("Module C: augment on empty matching with multiple edges", {
  # 3x3, no matching
  row_match <- c(0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L)
  
  # Single edge path
  edges <- matrix(c(1L, 1L), nrow = 1, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  expect_equal(result$row_match, c(1L, 0L, 0L))
  expect_equal(result$col_match, c(1L, 0L, 0L))
  expect_equal(sum(result$row_match > 0), 1)
})

test_that("Module C: augment correctly computes symmetric difference", {
  # Direct test of symmetric difference property
  # M = {(0,0), (1,1)}
  row_match <- c(1L, 2L, 0L)
  col_match <- c(1L, 2L, 0L)
  
  # P = {(0,0), (1,2), (2,2)}
  # M Δ P = (M - P) ∪ (P - M) = {(1,1)} ∪ {(1,2), (2,2)} = {(1,1), (1,2), (2,2)}
  # But wait - that would violate matching property!
  # Let's use a proper augmenting path instead
  
  # P = {(1,1), (1,2), (2,2)} should give M' = {(0,0), (1,2), (2,2)}
  # Actually, let's think more carefully...
  
  # M = {(0,0), (1,1)}
  # Augmenting path edges: (2,1), (1,1), (1,2)
  # This removes (1,1) and adds (2,1), (1,2)
  # Result: {(0,0), (2,1), (1,2)}
  
  edges <- matrix(c(
    3L, 2L,  # (2,1)
    2L, 2L,  # (1,1)
    2L, 3L   # (1,2)
  ), nrow = 3, byrow = TRUE)
  
  result <- gt_augment_along_path(edges, row_match, col_match)
  
  expect_equal(result$row_match, c(1L, 3L, 2L))
  expect_equal(sum(result$row_match > 0), 3)
})
