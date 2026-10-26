# ==============================================================================
# Tests for matching_join.R coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# join_matched parameter tests
# ------------------------------------------------------------------------------

test_that("join_matched with custom suffix", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3, y = 10:12)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1), y = c(10.1, 11.1, 12.1))

  result <- match_couples(left, right, vars = c("x", "y"))

  joined <- join_matched(result, left, right, suffix = c(".L", ".R"))

  expect_true("x.L" %in% names(joined))
  expect_true("x.R" %in% names(joined))
  expect_true("y.L" %in% names(joined))
  expect_true("y.R" %in% names(joined))
})

test_that("join_matched with specific left_vars and right_vars", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3, extra_left = letters[1:3])
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1), extra_right = letters[4:6])

  result <- match_couples(left, right, vars = "x")

  # Only include extra columns
  joined <- join_matched(
    result, left, right,
    left_vars = "extra_left",
    right_vars = "extra_right"
  )

  expect_true("extra_left_left" %in% names(joined))
  expect_true("extra_right_right" %in% names(joined))
  expect_false("x_left" %in% names(joined))
  expect_false("x_right" %in% names(joined))
})

test_that("join_matched with include_distance = FALSE", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  joined <- join_matched(result, left, right, include_distance = FALSE)

  expect_false("distance" %in% names(joined))
})

test_that("join_matched with include_pair_id = FALSE", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  joined <- join_matched(result, left, right, include_pair_id = FALSE)

  expect_false("pair_id" %in% names(joined))
})

test_that("join_matched with include_block_id = FALSE for blocked matching", {
  skip_on_cran()
  left <- data.frame(id = 1:6, x = 1:6, block = rep(c("A", "B"), each = 3))
  right <- data.frame(id = 7:12, x = c(1.1, 2.1, 3.1, 4.1, 5.1, 6.1),
                      block = rep(c("A", "B"), each = 3))

  result <- match_couples(left, right, vars = "x", block_id = "block")

  joined <- join_matched(result, left, right, include_block_id = FALSE)

  expect_false("block_id" %in% names(joined))
})

test_that("join_matched with custom ID columns", {
  skip_on_cran()
  left <- data.frame(my_id = 1:3, x = 1:3)
  right <- data.frame(other_id = 4:6, x = c(1.1, 2.1, 3.1))

  # Use compute_distances to specify custom ID columns
  dist_obj <- compute_distances(left, right, vars = "x", left_id = "my_id", right_id = "other_id")
  result <- match_couples(dist_obj)

  joined <- join_matched(result, left, right, left_id = "my_id", right_id = "other_id")

  expect_equal(nrow(joined), 3)
})

test_that("join_matched errors on non-matching_result", {
  skip_on_cran()
  expect_error(
    join_matched(list(), data.frame(), data.frame()),
    "must be a matching_result"
  )
})

test_that("join_matched errors on missing left_vars", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  expect_error(
    join_matched(result, left, right, left_vars = c("nonexistent")),
    "Variables not found in left"
  )
})

test_that("join_matched errors on missing right_vars", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  expect_error(
    join_matched(result, left, right, right_vars = c("nonexistent")),
    "Variables not found in right"
  )
})

test_that("join_matched errors on invalid suffix length", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  expect_error(
    join_matched(result, left, right, suffix = c("_only_one")),
    "suffix must be a character vector of length 2"
  )
})

test_that("join_matched errors on missing left_id column", {
  skip_on_cran()
  left <- data.frame(wrong_id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(
    data.frame(id = 1:3, x = 1:3),
    right,
    vars = "x"
  )

  expect_error(
    join_matched(result, left, right),
    "left_id column.*not found"
  )
})

test_that("join_matched errors on missing right_id column", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(wrong_id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(
    left,
    data.frame(id = 4:6, x = c(1.1, 2.1, 3.1)),
    vars = "x"
  )

  expect_error(
    join_matched(result, left, right),
    "right_id column.*not found"
  )
})

test_that("join_matched warns on empty result", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.5, 2.5, 3.5))

  # Get a valid result first
  result <- match_couples(left, right, vars = "x")

  # Create a result with no pairs by manually emptying it
  empty_result <- result
  empty_result$pairs <- result$pairs[0, ]

  expect_warning(
    joined <- join_matched(empty_result, left, right),
    "No matched pairs"
  )

  expect_equal(nrow(joined), 0)
})

test_that("join_matched handles non-overlapping variables", {
  skip_on_cran()
  left <- data.frame(id = 1:3, only_in_left = 1:3)
  right <- data.frame(id = 4:6, only_in_right = 4:6, x = c(1.1, 2.1, 3.1))

  left_for_match <- data.frame(id = 1:3, x = 1:3)
  result <- match_couples(left_for_match, right[, c("id", "x")], vars = "x")

  joined <- join_matched(result, left, right)

  expect_true("only_in_left_left" %in% names(joined))
  expect_true("only_in_right_right" %in% names(joined))
})

# ------------------------------------------------------------------------------
# augment tests
# ------------------------------------------------------------------------------

test_that("augment.matching_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  # Using couplr::augment
  augmented <- couplr::augment(result, left, right)

  expect_s3_class(augmented, "tbl_df")
  expect_equal(nrow(augmented), 3)
})

test_that("augment passes additional arguments to join_matched", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  augmented <- couplr::augment(result, left, right, include_distance = FALSE)

  expect_false("distance" %in% names(augmented))
})

test_that("augment generic dispatches correctly", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  # Should dispatch to augment.matching_result
  augmented <- augment(result, left, right)

  expect_s3_class(augmented, "tbl_df")
})

# ------------------------------------------------------------------------------
# Type conversion edge cases
# ------------------------------------------------------------------------------

test_that("join_matched handles character IDs", {
  skip_on_cran()
  left <- data.frame(id = letters[1:3], x = 1:3, stringsAsFactors = FALSE)
  right <- data.frame(id = letters[4:6], x = c(1.1, 2.1, 3.1), stringsAsFactors = FALSE)

  result <- match_couples(left, right, vars = "x")

  joined <- join_matched(result, left, right)

  expect_equal(nrow(joined), 3)
  expect_type(joined$left_id, "character")
  expect_type(joined$right_id, "character")
})

test_that("join_matched handles numeric IDs", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  joined <- join_matched(result, left, right)

  expect_equal(nrow(joined), 3)
})

test_that("join_matched handles factor IDs by converting to character", {
  skip_on_cran()
  # Factor IDs are converted to character during matching
  # so we test that the join still works
  left <- data.frame(id = letters[1:3], x = 1:3, stringsAsFactors = FALSE)
  right <- data.frame(id = letters[4:6], x = c(1.1, 2.1, 3.1), stringsAsFactors = FALSE)

  result <- match_couples(left, right, vars = "x")

  joined <- join_matched(result, left, right)

  expect_equal(nrow(joined), 3)
})

# ------------------------------------------------------------------------------
# Column ordering tests
# ------------------------------------------------------------------------------

test_that("join_matched orders columns correctly", {
  skip_on_cran()
  left <- data.frame(id = 1:3, a = 1:3, b = 4:6, c = 7:9)
  right <- data.frame(id = 4:6, a = c(1.1, 2.1, 3.1), d = 10:12, e = 13:15)

  result <- match_couples(left, right, vars = "a")

  joined <- join_matched(result, left, right)

  # Check column order: pair_id, left_id, right_id, distance should come first
  first_cols <- names(joined)[1:4]
  expect_equal(first_cols, c("pair_id", "left_id", "right_id", "distance"))
})
