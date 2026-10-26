# ==============================================================================
# Coverage tests for matching_parallel.R and matching_join.R
# ==============================================================================

# ------------------------------------------------------------------------------
# matching_parallel.R edge cases
# ------------------------------------------------------------------------------

test_that("setup_parallel returns early for FALSE", {
  result <- couplr:::setup_parallel(parallel = FALSE)
  expect_false(result$setup)
  expect_null(result$original_plan)
})

test_that("setup_parallel with TRUE and available packages", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  result <- couplr:::setup_parallel(parallel = TRUE, n_workers = 2)
  expect_true(result$setup)
  expect_true(!is.null(result$original_plan))
  # Restore
  couplr:::restore_parallel(result)
})

test_that("setup_parallel with character plan name", {
  skip_on_cran()
  result <- couplr:::setup_parallel(parallel = "sequential")
  expect_true(is.list(result))
  if (result$setup) {
    couplr:::restore_parallel(result)
  }
})

test_that("restore_parallel does nothing when setup is FALSE", {
  state <- list(setup = FALSE, original_plan = NULL)
  expect_silent(couplr:::restore_parallel(state))
})

test_that("parallel_lapply uses future when parallel=TRUE", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  result <- couplr:::parallel_lapply(1:3, function(x) x^2, parallel = TRUE)
  expect_equal(result, list(1, 4, 9))
})

test_that("parallel_lapply uses lapply when parallel=FALSE", {
  result <- couplr:::parallel_lapply(1:3, function(x) x^2, parallel = FALSE)
  expect_equal(result, list(1, 4, 9))
})

test_that("match_blocks_parallel handles empty left block", {
  # Block A: 3 left, 1 right -> 1 match
  # Block B: 0 left, 2 right -> 0 matches
  left <- data.frame(id = 1:3, block = c("A", "A", "A"), x = 1:3)
  right <- data.frame(id = 4:6, block = c("A", "B", "B"), x = 4:6)

  result <- couplr:::match_blocks_parallel(
    blocks = c("A", "B"),
    left = left, right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block",
    vars = "x", distance = "euclidean",
    weights = 1, scale = "none",
    max_distance = Inf, calipers = NULL,
    method = "jv", parallel = FALSE
  )

  expect_equal(nrow(result$pairs), 1)  # Only 1 right in A, so 1 match
  expect_true(length(result$unmatched$right) >= 2)  # B's right units unmatched
})

test_that("match_blocks_parallel handles empty right block", {
  left <- data.frame(id = 1:4, block = c("A", "A", "B", "B"), x = 1:4)
  right <- data.frame(id = 5:6, block = c("A", "A"), x = 5:6)

  # Block B has no right entries
  result <- couplr:::match_blocks_parallel(
    blocks = c("A", "B"),
    left = left, right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block",
    vars = "x", distance = "euclidean",
    weights = 1, scale = "none",
    max_distance = Inf, calipers = NULL,
    method = "jv", parallel = FALSE
  )

  expect_equal(nrow(result$pairs), 2)  # Only A matches
  expect_true(length(result$unmatched$left) >= 2)  # B's left units unmatched
})

test_that("greedy_blocks_parallel handles empty left block", {
  # Block A: 3 left, 1 right -> 1 match
  # Block B: 0 left, 2 right -> 0 matches
  left <- data.frame(id = 1:3, block = c("A", "A", "A"), x = 1:3)
  right <- data.frame(id = 4:6, block = c("A", "B", "B"), x = 4:6)

  result <- couplr:::greedy_blocks_parallel(
    blocks = c("A", "B"),
    left = left, right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block",
    vars = "x", distance = "euclidean",
    weights = 1, scale = "none",
    max_distance = Inf, calipers = NULL,
    strategy = "sorted", parallel = FALSE
  )

  expect_equal(nrow(result$pairs), 1)  # Only 1 right in A
})

test_that("greedy_blocks_parallel handles empty right block", {
  left <- data.frame(id = 1:4, block = c("A", "A", "B", "B"), x = 1:4)
  right <- data.frame(id = 5:6, block = c("A", "A"), x = 5:6)

  result <- couplr:::greedy_blocks_parallel(
    blocks = c("A", "B"),
    left = left, right = right,
    left_ids = as.character(left$id),
    right_ids = as.character(right$id),
    block_col = "block",
    vars = "x", distance = "euclidean",
    weights = 1, scale = "none",
    max_distance = Inf, calipers = NULL,
    strategy = "sorted", parallel = FALSE
  )

  expect_equal(nrow(result$pairs), 2)
})

# ------------------------------------------------------------------------------
# matching_join.R edge cases
# ------------------------------------------------------------------------------

test_that("join_matched errors on non-matching_result", {
  expect_error(
    couplr::join_matched(data.frame(), data.frame(id = 1), data.frame(id = 1)),
    "matching_result"
  )
})

test_that("join_matched errors on non-data frame left", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  expect_error(
    couplr::join_matched(result, list(x = 1), right),
    "data frames"
  )
})

test_that("join_matched errors on non-data frame right", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  expect_error(
    couplr::join_matched(result, left, list(x = 1)),
    "data frames"
  )
})

test_that("join_matched errors on missing left_id column", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  expect_error(
    couplr::join_matched(result, left, right, left_id = "missing_col"),
    "not found in left"
  )
})

test_that("join_matched errors on missing right_id column", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  expect_error(
    couplr::join_matched(result, left, right, right_id = "missing_col"),
    "not found in right"
  )
})

test_that("join_matched errors on wrong suffix length", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  expect_error(
    couplr::join_matched(result, left, right, suffix = "_only_one"),
    "length 2"
  )
})

test_that("join_matched warns on empty pairs", {
  # Create a result with empty pairs
  result <- list(
    pairs = tibble::tibble(
      left_id = character(0),
      right_id = character(0),
      distance = numeric(0)
    ),
    info = list()
  )
  class(result) <- c("matching_result", "list")

  expect_warning(
    couplr::join_matched(result, data.frame(id = 1), data.frame(id = 2)),
    "No matched pairs"
  )
})

test_that("join_matched errors on missing left_vars", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  expect_error(
    couplr::join_matched(result, left, right, left_vars = c("x", "missing_var")),
    "not found in left"
  )
})

test_that("join_matched errors on missing right_vars", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  expect_error(
    couplr::join_matched(result, left, right, right_vars = c("x", "missing_var")),
    "not found in right"
  )
})

test_that("join_matched works with custom suffixes", {
  left <- data.frame(id = 1:3, x = 1:3, y = 4:6)
  right <- data.frame(id = 4:6, x = 7:9, z = 10:12)
  result <- couplr::match_couples(left, right, vars = "x")

  joined <- couplr::join_matched(result, left, right, suffix = c("_treated", "_control"))
  expect_true("x_treated" %in% names(joined))
  expect_true("x_control" %in% names(joined))
  expect_true("y_treated" %in% names(joined))
  expect_true("z_control" %in% names(joined))
})

test_that("join_matched includes/excludes optional columns", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  # Without distance
  joined <- couplr::join_matched(result, left, right, include_distance = FALSE)
  expect_false("distance" %in% names(joined))

  # Without pair_id
  joined <- couplr::join_matched(result, left, right, include_pair_id = FALSE)
  expect_false("pair_id" %in% names(joined))
})

test_that("join_matched handles numeric IDs correctly", {
  left <- data.frame(id = 1:3, x = c(1.0, 2.0, 3.0))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))
  result <- couplr::match_couples(left, right, vars = "x")

  joined <- couplr::join_matched(result, left, right)
  expect_equal(nrow(joined), 3)
  expect_true(all(c("x_left", "x_right") %in% names(joined)))
})

test_that("augment.matching_result works", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  result <- couplr::match_couples(left, right, vars = "x")

  # Test via couplr::augment
  joined <- couplr::augment(result, left, right)
  expect_s3_class(joined, "tbl_df")
  expect_equal(nrow(joined), 3)
})

test_that("join_matched with specific left_vars and right_vars", {
  left <- data.frame(id = 1:3, x = 1:3, y = 4:6, z = 7:9)
  right <- data.frame(id = 4:6, x = 10:12, a = 13:15, b = 16:18)
  result <- couplr::match_couples(left, right, vars = "x")

  joined <- couplr::join_matched(
    result, left, right,
    left_vars = c("x", "y"),
    right_vars = c("x", "a")
  )

  expect_true("x_left" %in% names(joined))
  expect_true("y_left" %in% names(joined))
  expect_true("x_right" %in% names(joined))
  expect_true("a_right" %in% names(joined))
  expect_false("z_left" %in% names(joined))
  expect_false("b_right" %in% names(joined))
})
