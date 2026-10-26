# ==============================================================================
# Coverage tests for matching_blocks.R
# ==============================================================================

# ------------------------------------------------------------------------------
# matchmaker() edge cases
# ------------------------------------------------------------------------------

test_that("matchmaker with block_type=none works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  result <- couplr::matchmaker(left, right, block_type = "none")
  expect_s3_class(result, "matchmaker_result")
  expect_equal(result$info$block_type, "none")
  expect_equal(result$info$n_blocks_kept, 1)
  expect_equal(unique(result$left$block_id), "all")
})

test_that("matchmaker errors on missing block_by for group type", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  expect_error(
    couplr::matchmaker(left, right, block_type = "group", block_by = NULL),
    "must specify block_by"
  )
})

test_that("matchmaker errors on missing block_vars for cluster type", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  expect_error(
    couplr::matchmaker(left, right, block_type = "cluster", block_vars = NULL),
    "must specify block_vars"
  )
})

test_that("matchmaker with multiple block_by variables", {
  skip_on_cran()
  left <- data.frame(
    id = 1:8,
    region = rep(c("A", "B"), each = 4),
    year = rep(c(2020, 2021), 4),
    x = 1:8
  )
  right <- data.frame(
    id = 9:16,
    region = rep(c("A", "B"), each = 4),
    year = rep(c(2020, 2021), 4),
    x = 9:16
  )

  result <- couplr::matchmaker(left, right, block_type = "group", block_by = c("region", "year"))
  expect_s3_class(result, "matchmaker_result")
  expect_true(all(grepl("_", result$left$block_id)))  # Combined IDs
})

test_that("matchmaker with hclust clustering", {
  skip_on_cran()
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- couplr::matchmaker(
    left, right,
    block_type = "cluster",
    block_vars = "x",
    block_method = "hclust",
    n_blocks = 3
  )
  expect_s3_class(result, "matchmaker_result")
  expect_equal(result$info$block_type, "cluster")
})

test_that("matchmaker errors on unknown clustering method", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  expect_error(
    couplr::matchmaker(left, right, block_type = "cluster", block_vars = "x",
                       block_method = "unknown_method"),
    "Unknown clustering method"
  )
})

test_that("matchmaker with auto n_blocks selection", {
  skip_on_cran()
  set.seed(123)
  left <- data.frame(id = 1:20, x = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20))

  result <- couplr::matchmaker(
    left, right,
    block_type = "cluster",
    block_vars = "x",
    n_blocks = NULL  # Should auto-select
  )
  expect_s3_class(result, "matchmaker_result")
  expect_true(result$info$n_blocks_kept >= 2)
})

test_that("matchmaker filters small blocks", {
  skip_on_cran()
  left <- data.frame(
    id = 1:6,
    group = c("A", "A", "A", "A", "B", "C"),  # B and C have only 1 left
    x = 1:6
  )
  right <- data.frame(
    id = 7:12,
    group = c("A", "A", "A", "A", "B", "C"),
    x = 7:12
  )

  result <- couplr::matchmaker(left, right, block_type = "group", block_by = "group",
                       min_left = 2, min_right = 2)

  # B and C should be dropped (only 1 each)
  expect_true("A" %in% result$left$block_id)
  expect_false("B" %in% result$left$block_id)
  expect_false("C" %in% result$left$block_id)
  expect_true(length(result$dropped$blocks) >= 2)
})

test_that("matchmaker filters imbalanced blocks", {
  skip_on_cran()
  left <- data.frame(
    id = 1:10,
    group = c(rep("A", 8), "B", "B"),  # A: 8 left, B: 2 left
    x = 1:10
  )
  right <- data.frame(
    id = 11:14,
    group = c("A", "A", "B", "B"),  # A: 2 right, B: 2 right
    x = 11:14
  )

  result <- couplr::matchmaker(
    left, right,
    block_type = "group",
    block_by = "group",
    drop_imbalanced = TRUE,
    imbalance_threshold = 0.5
  )

  # A is highly imbalanced (8 vs 2), should be dropped
  expect_false("A" %in% result$left$block_id)
  expect_true("B" %in% result$left$block_id)
})

test_that("matchmaker with return_dropped=FALSE", {
  skip_on_cran()
  left <- data.frame(id = 1:5, group = c("A", "A", "A", "B", "C"), x = 1:5)
  right <- data.frame(id = 6:10, group = c("A", "A", "A", "A", "A"), x = 6:10)

  result <- couplr::matchmaker(left, right, block_type = "group", block_by = "group",
                       min_right = 2, return_dropped = FALSE)

  expect_null(result$dropped)
})

# ------------------------------------------------------------------------------
# assign_blocks_group() edge cases
# ------------------------------------------------------------------------------

test_that("assign_blocks_group errors on missing left variables", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6, group = c("A", "B", "C"))

  expect_error(
    couplr:::assign_blocks_group(left, right, block_by = "group"),
    "missing block_by"
  )
})

test_that("assign_blocks_group errors on missing right variables", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3, group = c("A", "B", "C"))
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(
    couplr:::assign_blocks_group(left, right, block_by = "group"),
    "missing block_by"
  )
})

# ------------------------------------------------------------------------------
# filter_blocks() edge cases
# ------------------------------------------------------------------------------

test_that("filter_blocks handles empty blocks in one side", {
  skip_on_cran()
  left <- data.frame(id = 1:3, block_id = c("A", "A", "A"))
  right <- data.frame(id = 4:6, block_id = c("A", "B", "B"))

  result <- couplr:::filter_blocks(left, right, min_left = 1, min_right = 1,
                                   drop_imbalanced = FALSE, imbalance_threshold = Inf)

  # B has no left, should be dropped
  expect_true("B" %in% result$dropped$blocks)
})

# ------------------------------------------------------------------------------
# summarize_blocks() edge cases
# ------------------------------------------------------------------------------

test_that("summarize_blocks computes variable means", {
  skip_on_cran()
  left <- data.frame(
    block_id = c("A", "A", "B", "B"),
    x = c(1, 3, 10, 20)
  )
  right <- data.frame(
    block_id = c("A", "A", "B", "B"),
    x = c(2, 4, 15, 25)
  )

  result <- couplr:::summarize_blocks(left, right, block_vars = "x")
  expect_true("mean_x" %in% names(result))
})

# ------------------------------------------------------------------------------
# print.matchmaker_result
# ------------------------------------------------------------------------------

test_that("print.matchmaker_result works with dropped blocks", {
  skip_on_cran()
  left <- data.frame(id = 1:6, group = c("A", "A", "A", "B", "C", "D"), x = 1:6)
  right <- data.frame(id = 7:10, group = c("A", "A", "A", "A"), x = 7:10)

  result <- couplr::matchmaker(left, right, block_type = "group", block_by = "group",
                       min_right = 2)

  expect_output(print(result), "Matchmaker Result")
  expect_output(print(result), "Blocks dropped")
  expect_output(print(result), "Dropped blocks")
})

test_that("print.matchmaker_result works without dropped blocks", {
  skip_on_cran()
  left <- data.frame(id = 1:4, group = c("A", "A", "B", "B"), x = 1:4)
  right <- data.frame(id = 5:8, group = c("A", "A", "B", "B"), x = 5:8)

  result <- couplr::matchmaker(left, right, block_type = "group", block_by = "group")

  expect_output(print(result), "Matchmaker Result")
  expect_output(print(result), "Block type: group")
})
