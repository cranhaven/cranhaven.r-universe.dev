# tests/testthat/test_gabow_tarjan_moduleD.R

test_that("Module D: simple case with two vertex-disjoint paths", {
  # 2x2, all edges eligible, no initial matching
  eq_graph <- list(
    c(1L, 2L),  # row 0 -> cols 0,1
    c(1L, 2L)   # row 1 -> cols 0,1
  )
  
  row_match <- c(0L, 0L)  # NIL
  col_match <- c(0L, 0L)  # NIL
  
  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)
  
  # Should find 2 vertex-disjoint augmenting paths
  expect_equal(length(paths), 2)
  
  # Each path should be a single edge (free row to free col)
  expect_equal(nrow(paths[[1]]), 1)
  expect_equal(nrow(paths[[2]]), 1)
  
  # Paths should use different rows and columns
  all_rows <- c(paths[[1]][,1], paths[[2]][,1])
  all_cols <- c(paths[[1]][,2], paths[[2]][,2])
  expect_equal(length(unique(all_rows)), 2)
  expect_equal(length(unique(all_cols)), 2)
})

test_that("Module D: augmenting path with alternating edges", {
  # 3x3, initial matching: {(0,0), (1,1)}
  # Free: row 2, col 2
  eq_graph <- list(
    c(1L),      # row 0 -> col 0 (matched)
    c(2L, 3L),  # row 1 -> cols 1 (matched), 2 (free)
    c(2L)       # row 2 -> col 1 (matched)
  )
  
  row_match <- c(1L, 2L, 0L)  # 0->0, 1->1, 2->NIL
  col_match <- c(1L, 2L, 0L)  # 0->0, 1->1, 2->NIL
  
  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)
  
  # Should find exactly one augmenting path. The current C++ representation
  # stores only the new selected edges: row 2 -> col 1, row 1 -> col 2.
  expect_equal(length(paths), 1)
  
  path <- paths[[1]]
  expect_equal(nrow(path), 2)
  
  # Check the path structure (0-based in comments, 1-based in R)
  expect_equal(path[1, 1], 3L)  # row 2
  expect_equal(path[1, 2], 2L)  # col 1
  expect_equal(path[2, 1], 2L)  # row 1
  expect_equal(path[2, 2], 3L)  # col 2
})

test_that("Module D: no augmenting paths when matching is perfect", {
  # 2x2 with perfect matching
  eq_graph <- list(
    c(1L),  # row 0 -> col 0 (matched)
    c(2L)   # row 1 -> col 1 (matched)
  )
  
  row_match <- c(1L, 2L)
  col_match <- c(1L, 2L)
  
  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)
  
  # No augmenting paths should exist
  expect_equal(length(paths), 0)
})

test_that("Module D: no augmenting paths when no eligible edges to free vertices", {
  # 3x3, matching: {(0,0), (1,1)}
  # Row 2 and col 2 are free, but no eligible edges connect them
  eq_graph <- list(
    c(1L),   # row 0 -> col 0 (matched)
    c(2L),   # row 1 -> col 1 (matched)
    c(1L)    # row 2 -> col 0 (matched to row 0, not free)
  )
  
  row_match <- c(1L, 2L, 0L)
  col_match <- c(1L, 2L, 0L)
  
  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)
  
  # No augmenting paths (free row 2 can't reach free col 2)
  expect_equal(length(paths), 0)
})

test_that("Module D: vertex-disjoint property is maintained", {
  # 4x4, no matching, all edges eligible
  eq_graph <- list(
    c(1L, 2L, 3L, 4L),
    c(1L, 2L, 3L, 4L),
    c(1L, 2L, 3L, 4L),
    c(1L, 2L, 3L, 4L)
  )
  
  row_match <- c(0L, 0L, 0L, 0L)
  col_match <- c(0L, 0L, 0L, 0L)
  
  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)
  
  # Should find 4 paths (maximal)
  expect_equal(length(paths), 4)
  
  # Collect all vertices used
  all_rows <- integer(0)
  all_cols <- integer(0)
  for (path in paths) {
    all_rows <- c(all_rows, path[, 1])
    all_cols <- c(all_cols, path[, 2])
  }
  
  # Check vertex-disjoint property: no duplicates
  expect_equal(length(all_rows), length(unique(all_rows)))
  expect_equal(length(all_cols), length(unique(all_cols)))
})

test_that("Module D: single-edge augmenting paths", {
  # 3x3, partial matching leaving one free row and one free col
  eq_graph <- list(
    c(1L),   # row 0 -> col 0 (matched)
    c(2L),   # row 1 -> col 1 (matched)
    c(3L)    # row 2 -> col 2 (free->free)
  )
  
  row_match <- c(1L, 2L, 0L)
  col_match <- c(1L, 2L, 0L)
  
  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)
  
  expect_equal(length(paths), 1)
  expect_equal(nrow(paths[[1]]), 1)
  expect_equal(paths[[1]][1, 1], 3L)  # row 2
  expect_equal(paths[[1]][1, 2], 3L)  # col 2
})

test_that("Module D: multiple alternating paths", {
  # 4x4 with complex structure
  eq_graph <- list(
    c(1L, 2L),     # row 0 -> cols 0,1
    c(1L, 2L, 3L), # row 1 -> cols 0,1,2
    c(2L, 3L),     # row 2 -> cols 1,2
    c(4L)          # row 3 -> col 3 (will be simple path)
  )

  # Initial matching: {(0,0), (1,1)}
  row_match <- c(1L, 2L, 0L, 0L)
  col_match <- c(1L, 2L, 0L, 0L)

  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)

  # Should find 2 paths: one for row3->col3, one more complex
  expect_equal(length(paths), 2)

  # Check that paths are vertex-disjoint ACROSS paths
  # Note: Within an alternating path, vertices can appear multiple times
  # (e.g., path [(2,1), (1,1), (1,2)] has row 1 and col 1 appearing twice)
  # We check that no vertex appears in BOTH paths

  # Get the set of vertices (rows and cols) used in each path
  path1_rows <- unique(paths[[1]][, 1])
  path1_cols <- unique(paths[[1]][, 2])
  path2_rows <- unique(paths[[2]][, 1])
  path2_cols <- unique(paths[[2]][, 2])

  # Check no row overlap between paths
  expect_equal(length(intersect(path1_rows, path2_rows)), 0)

  # Check no column overlap between paths
  expect_equal(length(intersect(path1_cols, path2_cols)), 0)
})

test_that("Module D: returns empty list not NULL when no paths", {
  eq_graph <- list(c(1L), c(2L))
  row_match <- c(1L, 2L)
  col_match <- c(1L, 2L)
  
  paths <- gt_find_maximal_augmenting_paths(eq_graph, row_match, col_match)
  
  expect_type(paths, "list")
  expect_equal(length(paths), 0)
})
