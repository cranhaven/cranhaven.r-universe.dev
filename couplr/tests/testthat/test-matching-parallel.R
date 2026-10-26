# ==============================================================================
# Tests for parallel matching helpers (matching_parallel.R)
# ==============================================================================

test_that("can_parallelize returns logical", {
  result <- couplr:::can_parallelize()
  expect_type(result, "logical")
})

test_that("can_parallelize returns TRUE when future packages installed", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  result <- couplr:::can_parallelize()
  expect_true(result)
})

test_that("setup_parallel returns list with correct structure when FALSE", {
  result <- couplr:::setup_parallel(parallel = FALSE)

  expect_type(result, "list")
  expect_true("setup" %in% names(result))
  expect_true("original_plan" %in% names(result))
  expect_false(result$setup)
  expect_null(result$original_plan)
})

test_that("setup_parallel sets up parallel when available", {
  skip_if_not(couplr:::can_parallelize())
  skip_on_cran()

  expect_message(
    result <- couplr:::setup_parallel(parallel = TRUE, n_workers = 2),
    "Parallel processing enabled"
  )

  expect_true(result$setup)
  expect_false(is.null(result$original_plan))

  # Clean up
  couplr:::restore_parallel(result)
})

test_that("restore_parallel restores original plan", {
  skip_if_not(couplr:::can_parallelize())
  skip_on_cran()

  # Get original plan
  original <- future::plan()

  # Setup parallel
  result <- suppressMessages(
    couplr:::setup_parallel(parallel = TRUE, n_workers = 2)
  )

  # Restore
  couplr:::restore_parallel(result)

  # Check plan is restored
  restored <- future::plan()
  expect_equal(class(original), class(restored))
})

test_that("parallel_lapply falls back to lapply when parallel FALSE", {
  result <- couplr:::parallel_lapply(1:5, function(x) x^2, parallel = FALSE)

  expect_equal(result, list(1, 4, 9, 16, 25))
})

test_that("parallel_lapply uses future when parallel TRUE and available", {
  skip_if_not(couplr:::can_parallelize())
  skip_on_cran()

  result <- couplr:::parallel_lapply(1:5, function(x) x^2, parallel = TRUE)

  expect_equal(result, list(1, 4, 9, 16, 25))
})

test_that("match_blocks_parallel works with sequential execution", {
  left <- data.frame(
    id = 1:10,
    block_id = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    block_id = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  result <- couplr:::match_blocks_parallel(
    blocks = c("A", "B"),
    left = left,
    right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block_id",
    vars = "x",
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    method = "auto",
    parallel = FALSE
  )

  expect_type(result, "list")
  expect_true("pairs" %in% names(result))
  expect_true("unmatched" %in% names(result))
  expect_true("block_summary" %in% names(result))
  expect_equal(nrow(result$block_summary), 2)
})

test_that("match_blocks_parallel handles empty blocks", {
  left <- data.frame(
    id = 1:5,
    block_id = rep("A", 5),
    x = rnorm(5)
  )
  right <- data.frame(
    id = 6:10,
    block_id = rep("B", 5),  # Different block - no overlap
    x = rnorm(5)
  )

  result <- couplr:::match_blocks_parallel(
    blocks = c("A", "B"),
    left = left,
    right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block_id",
    vars = "x",
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    method = "auto",
    parallel = FALSE
  )

  # Both blocks should be "empty" in terms of matches
  expect_equal(nrow(result$pairs), 0)
  expect_equal(nrow(result$block_summary), 2)
  expect_true(all(result$block_summary$n_matched == 0))
})

test_that("greedy_blocks_parallel works with sequential execution", {
  left <- data.frame(
    id = 1:10,
    block_id = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    block_id = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  result <- couplr:::greedy_blocks_parallel(
    blocks = c("A", "B"),
    left = left,
    right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block_id",
    vars = "x",
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    strategy = "sorted",
    parallel = FALSE
  )

  expect_type(result, "list")
  expect_true("pairs" %in% names(result))
  expect_true("unmatched" %in% names(result))
  expect_true("block_summary" %in% names(result))
  expect_equal(nrow(result$block_summary), 2)
})

test_that("greedy_blocks_parallel handles empty blocks", {
  left <- data.frame(
    id = 1:5,
    block_id = rep("A", 5),
    x = rnorm(5)
  )
  right <- data.frame(
    id = 6:10,
    block_id = rep("B", 5),
    x = rnorm(5)
  )

  result <- couplr:::greedy_blocks_parallel(
    blocks = c("A", "B"),
    left = left,
    right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block_id",
    vars = "x",
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    strategy = "sorted",
    parallel = FALSE
  )

  expect_equal(nrow(result$pairs), 0)
  expect_equal(nrow(result$block_summary), 2)
})

test_that("match_blocks_parallel with parallel produces same results", {
  skip_if_not(couplr:::can_parallelize())
  skip_on_cran()

  set.seed(123)
  left <- data.frame(
    id = 1:20,
    block_id = rep(c("A", "B", "C", "D"), each = 5),
    x = rnorm(20)
  )
  right <- data.frame(
    id = 21:40,
    block_id = rep(c("A", "B", "C", "D"), each = 5),
    x = rnorm(20)
  )

  result_seq <- couplr:::match_blocks_parallel(
    blocks = c("A", "B", "C", "D"),
    left = left,
    right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block_id",
    vars = "x",
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    method = "auto",
    parallel = FALSE
  )

  result_par <- couplr:::match_blocks_parallel(
    blocks = c("A", "B", "C", "D"),
    left = left,
    right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block_id",
    vars = "x",
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    method = "auto",
    parallel = TRUE
  )

  # Results should have same number of pairs
  expect_equal(nrow(result_seq$pairs), nrow(result_par$pairs))
  expect_equal(
    sum(result_seq$pairs$distance),
    sum(result_par$pairs$distance),
    tolerance = 1e-10
  )
})
