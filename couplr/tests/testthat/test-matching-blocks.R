# ==============================================================================
# Tests for blocking functions (matching_blocks.R)
# ==============================================================================

# ------------------------------------------------------------------------------
# matchmaker() tests
# ------------------------------------------------------------------------------

test_that("matchmaker with block_type='none' creates single block", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- matchmaker(left, right, block_type = "none")

  expect_s3_class(result, "matchmaker_result")
  expect_equal(unique(result$left$block_id), "all")
  expect_equal(unique(result$right$block_id), "all")
  expect_equal(nrow(result$block_summary), 1)
  expect_equal(result$info$n_blocks_kept, 1)
})

test_that("matchmaker with block_type='group' creates blocks by variable", {
  left <- data.frame(
    id = 1:10,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  result <- matchmaker(left, right, block_type = "group", block_by = "region")

  expect_s3_class(result, "matchmaker_result")
  expect_equal(sort(unique(result$left$block_id)), c("A", "B"))
  expect_equal(nrow(result$block_summary), 2)
  expect_equal(result$info$n_blocks_kept, 2)
})

test_that("matchmaker with multiple block_by variables", {
  left <- data.frame(
    id = 1:8,
    region = rep(c("A", "B"), each = 4),
    gender = rep(c("M", "F"), times = 4),
    x = rnorm(8)
  )
  right <- data.frame(
    id = 9:16,
    region = rep(c("A", "B"), each = 4),
    gender = rep(c("M", "F"), times = 4),
    x = rnorm(8)
  )

  result <- matchmaker(
    left, right,
    block_type = "group",
    block_by = c("region", "gender")
  )

  expect_s3_class(result, "matchmaker_result")
  # Should have 4 blocks: A_M, A_F, B_M, B_F
  expect_equal(result$info$n_blocks_kept, 4)
})

test_that("matchmaker with block_type='cluster' creates kmeans blocks", {
  set.seed(123)
  left <- data.frame(
    id = 1:20,
    x = c(rnorm(10, 0), rnorm(10, 10)),
    y = c(rnorm(10, 0), rnorm(10, 10))
  )
  right <- data.frame(
    id = 21:40,
    x = c(rnorm(10, 0), rnorm(10, 10)),
    y = c(rnorm(10, 0), rnorm(10, 10))
  )

  result <- matchmaker(
    left, right,
    block_type = "cluster",
    block_vars = c("x", "y"),
    n_blocks = 2
  )

  expect_s3_class(result, "matchmaker_result")
  expect_true(grepl("cluster", result$left$block_id[1]))
  expect_equal(result$info$n_blocks_kept, 2)
})

test_that("matchmaker with block_type='cluster' and hclust method", {
  set.seed(456)
  left <- data.frame(
    id = 1:20,
    x = c(rnorm(10, 0), rnorm(10, 10))
  )
  right <- data.frame(
    id = 21:40,
    x = c(rnorm(10, 0), rnorm(10, 10))
  )

  result <- matchmaker(
    left, right,
    block_type = "cluster",
    block_vars = "x",
    block_method = "hclust",
    n_blocks = 2
  )

  expect_s3_class(result, "matchmaker_result")
  expect_equal(result$info$n_blocks_kept, 2)
})

test_that("matchmaker errors when block_by missing for group type", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    matchmaker(left, right, block_type = "group"),
    "must specify block_by"
  )
})

test_that("matchmaker errors when block_vars missing for cluster type", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    matchmaker(left, right, block_type = "cluster"),
    "must specify block_vars"
  )
})

test_that("matchmaker errors when block_by variable missing from left", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, region = rep("A", 5), x = rnorm(5))

  expect_error(
    matchmaker(left, right, block_type = "group", block_by = "region"),
    "missing block_by"
  )
})

test_that("matchmaker errors when block_by variable missing from right", {
  left <- data.frame(id = 1:5, region = rep("A", 5), x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    matchmaker(left, right, block_type = "group", block_by = "region"),
    "missing block_by"
  )
})

test_that("matchmaker filters small blocks with min_left", {
  left <- data.frame(
    id = 1:8,
    region = c(rep("A", 5), rep("B", 2), "C"),
    x = rnorm(8)
  )
  right <- data.frame(
    id = 9:16,
    region = c(rep("A", 5), rep("B", 2), "C"),
    x = rnorm(8)
  )

  result <- matchmaker(
    left, right,
    block_type = "group",
    block_by = "region",
    min_left = 3,
    min_right = 3
  )

  # Only A should remain
  expect_equal(result$info$n_blocks_kept, 1)
  expect_equal(result$info$n_blocks_dropped, 2)
})

test_that("matchmaker drop_imbalanced filters imbalanced blocks", {
  left <- data.frame(
    id = 1:15,
    region = c(rep("A", 5), rep("B", 10)),
    x = rnorm(15)
  )
  right <- data.frame(
    id = 16:30,
    region = c(rep("A", 5), rep("B", 10)),
    x = rnorm(15)
  )

  # Without drop_imbalanced
  result1 <- matchmaker(
    left, right,
    block_type = "group",
    block_by = "region"
  )
  expect_equal(result1$info$n_blocks_kept, 2)

  # B is balanced (10:10), A is balanced (5:5) - so neither should be dropped
  result2 <- matchmaker(
    left, right,
    block_type = "group",
    block_by = "region",
    drop_imbalanced = TRUE,
    imbalance_threshold = 0.3
  )
  expect_equal(result2$info$n_blocks_kept, 2)
})

test_that("matchmaker returns dropped blocks when return_dropped=TRUE", {
  left <- data.frame(
    id = 1:6,
    region = c(rep("A", 5), "B"),  # B has only 1
    x = rnorm(6)
  )
  right <- data.frame(
    id = 7:12,
    region = c(rep("A", 5), "B"),
    x = rnorm(6)
  )

  result <- matchmaker(
    left, right,
    block_type = "group",
    block_by = "region",
    min_left = 2,
    return_dropped = TRUE
  )

  expect_false(is.null(result$dropped))
  expect_true("B" %in% result$dropped$blocks)
})

test_that("matchmaker handles n_blocks = NULL for clustering", {
  set.seed(789)
  left <- data.frame(
    id = 1:30,
    x = rnorm(30)
  )
  right <- data.frame(
    id = 31:60,
    x = rnorm(30)
  )

  result <- matchmaker(
    left, right,
    block_type = "cluster",
    block_vars = "x",
    n_blocks = NULL  # Auto-determine
  )

  expect_s3_class(result, "matchmaker_result")
  expect_true(result$info$n_blocks_kept >= 1)
})

# ------------------------------------------------------------------------------
# Internal functions tests
# ------------------------------------------------------------------------------

test_that("assign_blocks_group creates correct block IDs", {
  left <- data.frame(id = 1:4, region = c("A", "A", "B", "B"))
  right <- data.frame(id = 5:8, region = c("A", "B", "B", "A"))

  result <- couplr:::assign_blocks_group(left, right, "region")

  expect_equal(result$left$block_id, c("A", "A", "B", "B"))
  expect_equal(result$right$block_id, c("A", "B", "B", "A"))
  expect_equal(result$n_blocks_initial, 2)
})

test_that("assign_blocks_cluster creates cluster-prefixed block IDs", {
  set.seed(123)
  left <- data.frame(
    id = 1:10,
    x = c(rnorm(5, 0), rnorm(5, 10))
  )
  right <- data.frame(
    id = 11:20,
    x = c(rnorm(5, 0), rnorm(5, 10))
  )

  result <- couplr:::assign_blocks_cluster(
    left, right,
    block_vars = "x",
    method = "kmeans",
    n_blocks = 2
  )

  expect_true(all(grepl("^cluster_", result$left$block_id)))
  expect_true(all(grepl("^cluster_", result$right$block_id)))
  expect_equal(result$n_blocks_initial, 2)
})

test_that("assign_blocks_cluster errors on unknown method", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    couplr:::assign_blocks_cluster(
      left, right,
      block_vars = "x",
      method = "unknown_method",
      n_blocks = 2
    ),
    "Unknown clustering method"
  )
})

test_that("filter_blocks filters correctly by size", {
  left <- data.frame(
    id = 1:8,
    block_id = c(rep("A", 5), rep("B", 2), "C")
  )
  right <- data.frame(
    id = 9:16,
    block_id = c(rep("A", 5), rep("B", 2), "C")
  )

  result <- couplr:::filter_blocks(left, right, min_left = 3, min_right = 3,
                                   drop_imbalanced = FALSE, imbalance_threshold = Inf)

  expect_equal(nrow(result$left), 5)  # Only A
  expect_equal(nrow(result$right), 5)
  expect_true("B" %in% result$dropped$blocks)
  expect_true("C" %in% result$dropped$blocks)
})

test_that("filter_blocks handles empty blocks", {
  left <- data.frame(id = 1:5, block_id = rep("A", 5))
  right <- data.frame(id = 6:10, block_id = rep("B", 5))  # Different block

  result <- couplr:::filter_blocks(left, right, min_left = 1, min_right = 1,
                                   drop_imbalanced = FALSE, imbalance_threshold = Inf)

  # Both should be dropped because they have 0 on the other side
  expect_equal(nrow(result$left), 0)
  expect_equal(nrow(result$right), 0)
})

test_that("summarize_blocks creates correct summary", {
  left <- data.frame(
    id = 1:8,
    block_id = c(rep("A", 5), rep("B", 3))
  )
  right <- data.frame(
    id = 9:15,
    block_id = c(rep("A", 4), rep("B", 3))
  )

  result <- couplr:::summarize_blocks(left, right)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("n_left" %in% names(result))
  expect_true("n_right" %in% names(result))
  expect_equal(result$n_left[result$block_id == "A"], 5)
  expect_equal(result$n_right[result$block_id == "A"], 4)
})

test_that("summarize_blocks includes variable means when specified", {
  left <- data.frame(
    id = 1:6,
    block_id = c(rep("A", 3), rep("B", 3)),
    x = c(1, 2, 3, 10, 11, 12)
  )
  right <- data.frame(
    id = 7:12,
    block_id = c(rep("A", 3), rep("B", 3)),
    x = c(1, 2, 3, 10, 11, 12)
  )

  result <- couplr:::summarize_blocks(left, right, block_vars = "x")

  expect_true("mean_x" %in% names(result))
  expect_equal(result$mean_x[result$block_id == "A"], 2)
  expect_equal(result$mean_x[result$block_id == "B"], 11)
})

# ------------------------------------------------------------------------------
# Print method tests
# ------------------------------------------------------------------------------

test_that("print.matchmaker_result works", {
  left <- data.frame(
    id = 1:10,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  result <- matchmaker(left, right, block_type = "group", block_by = "region")

  expect_output(print(result), "Matchmaker Result")
  expect_output(print(result), "Block type")
  expect_output(print(result), "Blocks kept")
})

test_that("print.matchmaker_result shows dropped blocks", {
  left <- data.frame(
    id = 1:6,
    region = c(rep("A", 5), "B"),
    x = rnorm(6)
  )
  right <- data.frame(
    id = 7:12,
    region = c(rep("A", 5), "B"),
    x = rnorm(6)
  )

  result <- matchmaker(
    left, right,
    block_type = "group",
    block_by = "region",
    min_left = 2
  )

  expect_output(print(result), "Dropped blocks")
})

# ------------------------------------------------------------------------------
# Integration tests
# ------------------------------------------------------------------------------

test_that("matchmaker result works with match_couples", {
  left <- data.frame(
    id = 1:10,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
  result <- match_couples(blocks$left, blocks$right, vars = "x")

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 10)
  expect_true("block_id" %in% names(result$pairs))
})

test_that("matchmaker result works with greedy matching", {
  left <- data.frame(
    id = 1:10,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
  result <- match_couples(blocks$left, blocks$right, vars = "x", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 10)
})
