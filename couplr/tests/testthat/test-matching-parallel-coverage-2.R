# ==============================================================================
# Additional tests for matching_parallel.R to reach 90%+ coverage
# ==============================================================================

# Create test datasets with blocks
set.seed(789)
left_df <- data.frame(
  id = paste0("L", 1:30),
  age = rnorm(30, 40, 10),
  income = rnorm(30, 50000, 15000),
  block = rep(c("A", "B", "C"), each = 10),
  stringsAsFactors = FALSE
)
right_df <- data.frame(
  id = paste0("R", 1:30),
  age = rnorm(30, 42, 10),
  income = rnorm(30, 52000, 15000),
  block = rep(c("A", "B", "C"), each = 10),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# can_parallelize tests
# ------------------------------------------------------------------------------

test_that("can_parallelize returns logical", {
  skip_on_cran()
  result <- couplr:::can_parallelize()
  expect_type(result, "logical")
})

# ------------------------------------------------------------------------------
# setup_parallel tests
# ------------------------------------------------------------------------------

test_that("setup_parallel returns FALSE when parallel=FALSE", {
  skip_on_cran()
  result <- couplr:::setup_parallel(parallel = FALSE)

  expect_false(result$setup)
  expect_null(result$original_plan)
})

test_that("setup_parallel handles missing future package gracefully", {
  skip_on_cran()
  # If future is not installed, should fall back gracefully
  result <- suppressWarnings(couplr:::setup_parallel(parallel = TRUE))

  # Result depends on whether future is installed
  expect_type(result, "list")
  expect_true("setup" %in% names(result))
})

test_that("setup_parallel with parallel=TRUE when future available", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  # Store original plan
  original <- future::plan()

  result <- suppressMessages(couplr:::setup_parallel(parallel = TRUE))

  expect_type(result, "list")
  expect_true("setup" %in% names(result))

  # Restore original plan
  future::plan(original)
})

test_that("setup_parallel with character plan name", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  # Store original plan
  original <- future::plan()

  result <- suppressWarnings(couplr:::setup_parallel(parallel = "sequential"))

  expect_type(result, "list")

  # Restore original plan
  future::plan(original)
})

test_that("setup_parallel with invalid plan name warns", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  original <- future::plan()

  # Invalid plan name should warn
  expect_warning(
    result <- couplr:::setup_parallel(parallel = "invalid_plan_xyz"),
    "Could not set parallel plan"
  )

  future::plan(original)
})

# ------------------------------------------------------------------------------
# restore_parallel tests
# ------------------------------------------------------------------------------

test_that("restore_parallel does nothing when not set up", {
  skip_on_cran()
  state <- list(setup = FALSE, original_plan = NULL)

  # Should not error
  expect_silent(couplr:::restore_parallel(state))
})

test_that("restore_parallel restores plan when set up", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  original <- future::plan()
  state <- list(setup = TRUE, original_plan = original)

  # Should restore without error
  expect_silent(couplr:::restore_parallel(state))
})

# ------------------------------------------------------------------------------
# parallel_lapply tests
# ------------------------------------------------------------------------------

test_that("parallel_lapply works sequentially when parallel=FALSE", {
  skip_on_cran()
  result <- couplr:::parallel_lapply(1:5, function(x) x^2, parallel = FALSE)

  expect_equal(result, list(1, 4, 9, 16, 25))
})

test_that("parallel_lapply works with parallel=TRUE when future available", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  # Use sequential plan for testing
  original <- future::plan()
  future::plan(future::sequential)

  result <- couplr:::parallel_lapply(1:5, function(x) x^2, parallel = TRUE)

  expect_equal(result, list(1, 4, 9, 16, 25))

  future::plan(original)
})

# ------------------------------------------------------------------------------
# match_blocks_parallel tests
# ------------------------------------------------------------------------------

test_that("match_blocks_parallel works without parallel", {
  skip_on_cran()
  left_ids <- left_df$id
  right_ids <- right_df$id
  blocks <- unique(left_df$block)

  result <- couplr:::match_blocks_parallel(
    blocks = blocks,
    left = left_df,
    right = right_df,
    left_ids = left_ids,
    right_ids = right_ids,
    block_col = "block",
    vars = c("age"),
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    method = "hungarian",
    parallel = FALSE
  )

  expect_true(is.data.frame(result$pairs))
  expect_true("block_id" %in% names(result$pairs))
  expect_true(is.data.frame(result$block_summary))
})

test_that("match_blocks_parallel handles empty blocks", {
  skip_on_cran()
  # Create data where one block has no right units
  left_empty <- data.frame(
    id = paste0("L", 1:15),
    age = rnorm(15),
    block = c(rep("A", 10), rep("B", 5))
  )
  right_empty <- data.frame(
    id = paste0("R", 1:10),
    age = rnorm(10),
    block = rep("A", 10)  # No "B" units
  )

  result <- couplr:::match_blocks_parallel(
    blocks = c("A", "B"),
    left = left_empty,
    right = right_empty,
    left_ids = left_empty$id,
    right_ids = right_empty$id,
    block_col = "block",
    vars = c("age"),
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    method = "hungarian",
    parallel = FALSE
  )

  expect_true(is.data.frame(result$pairs))
  # Block B should have unmatched left units
  expect_true(any(grepl("L1[1-5]", result$unmatched$left)))
})

test_that("match_blocks_parallel returns empty pairs for all-empty result", {
  skip_on_cran()
  left_empty <- data.frame(
    id = character(0),
    age = numeric(0),
    block = character(0)
  )
  right_empty <- data.frame(
    id = character(0),
    age = numeric(0),
    block = character(0)
  )

  result <- couplr:::match_blocks_parallel(
    blocks = character(0),
    left = left_empty,
    right = right_empty,
    left_ids = character(0),
    right_ids = character(0),
    block_col = "block",
    vars = c("age"),
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    method = "hungarian",
    parallel = FALSE
  )

  expect_equal(nrow(result$pairs), 0)
})

# ------------------------------------------------------------------------------
# greedy_blocks_parallel tests
# ------------------------------------------------------------------------------

test_that("greedy_blocks_parallel works without parallel", {
  skip_on_cran()
  left_ids <- left_df$id
  right_ids <- right_df$id
  blocks <- unique(left_df$block)

  result <- couplr:::greedy_blocks_parallel(
    blocks = blocks,
    left = left_df,
    right = right_df,
    left_ids = left_ids,
    right_ids = right_ids,
    block_col = "block",
    vars = c("age"),
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    strategy = "row_best",
    parallel = FALSE
  )

  expect_true(is.data.frame(result$pairs))
  expect_true("block_id" %in% names(result$pairs))
  expect_true(is.data.frame(result$block_summary))
})

test_that("greedy_blocks_parallel handles empty blocks", {
  skip_on_cran()
  left_empty <- data.frame(
    id = paste0("L", 1:15),
    age = rnorm(15),
    block = c(rep("A", 10), rep("B", 5))
  )
  right_empty <- data.frame(
    id = paste0("R", 1:10),
    age = rnorm(10),
    block = rep("A", 10)  # No "B" units
  )

  result <- couplr:::greedy_blocks_parallel(
    blocks = c("A", "B"),
    left = left_empty,
    right = right_empty,
    left_ids = left_empty$id,
    right_ids = right_empty$id,
    block_col = "block",
    vars = c("age"),
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    strategy = "row_best",
    parallel = FALSE
  )

  expect_true(is.data.frame(result$pairs))
})

test_that("greedy_blocks_parallel with sorted strategy", {
  skip_on_cran()
  left_ids <- left_df$id
  right_ids <- right_df$id
  blocks <- unique(left_df$block)

  result <- couplr:::greedy_blocks_parallel(
    blocks = blocks,
    left = left_df,
    right = right_df,
    left_ids = left_ids,
    right_ids = right_ids,
    block_col = "block",
    vars = c("age"),
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    strategy = "sorted",
    parallel = FALSE
  )

  expect_true(is.data.frame(result$pairs))
})

test_that("greedy_blocks_parallel with pq strategy", {
  skip_on_cran()
  left_ids <- left_df$id
  right_ids <- right_df$id
  blocks <- unique(left_df$block)

  result <- couplr:::greedy_blocks_parallel(
    blocks = blocks,
    left = left_df,
    right = right_df,
    left_ids = left_ids,
    right_ids = right_ids,
    block_col = "block",
    vars = c("age"),
    distance = "euclidean",
    weights = NULL,
    scale = FALSE,
    max_distance = Inf,
    calipers = NULL,
    strategy = "pq",
    parallel = FALSE
  )

  expect_true(is.data.frame(result$pairs))
})

# ------------------------------------------------------------------------------
# Integration tests with match_couples and parallel
# ------------------------------------------------------------------------------

test_that("match_couples with parallel=TRUE works", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  # Store original plan
  original <- future::plan()
  future::plan(future::sequential)

  result <- suppressMessages(
    match_couples(
      left_df, right_df,
      vars = c("age"),
      block_id = "block",
      parallel = TRUE
    )
  )

  expect_s3_class(result, "matching_result")

  future::plan(original)
})

test_that("greedy matching with parallel=TRUE and blocking works", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  original <- future::plan()
  future::plan(future::sequential)

  result <- suppressMessages(
    match_couples(
      left_df, right_df,
      vars = c("age"),
      block_id = "block",
      parallel = TRUE
    , method = "greedy")
  )

  expect_s3_class(result, "matching_result")

  future::plan(original)
})
