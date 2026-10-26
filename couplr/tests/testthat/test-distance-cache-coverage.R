# ==============================================================================
# Tests for matching_distance_cache.R coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# compute_distances edge cases
# ------------------------------------------------------------------------------

test_that("compute_distances works with blocking", {
  skip_on_cran()
  left <- data.frame(id = 1:6, x = 1:6, block = rep(c("A", "B"), each = 3))
  right <- data.frame(id = 7:12, x = 7:12, block = rep(c("A", "B"), each = 3))

  expect_message(
    dist_obj <- compute_distances(left, right, vars = "x", block_id = "block"),
    "Block information stored"
  )

  expect_true(is_distance_object(dist_obj))
  expect_equal(dist_obj$block_id, "block")
})

test_that("compute_distances with auto_scale applies preprocessing", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c(100, 200, 300, 400, 500))
  right <- data.frame(id = 6:10, x = c(1.5, 2.5, 3.5, 4.5, 5.5), y = c(150, 250, 350, 450, 550))

  dist_obj <- compute_distances(left, right, vars = c("x", "y"), auto_scale = TRUE)

  expect_true(is_distance_object(dist_obj))
  expect_true(dist_obj$metadata$auto_scale)
})

test_that("compute_distances with weights", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3, y = 1:3)
  right <- data.frame(id = 4:6, x = 4:6, y = 4:6)

  dist_obj <- compute_distances(left, right, vars = c("x", "y"), weights = c(2, 1))

  expect_true(is_distance_object(dist_obj))
  expect_equal(dist_obj$metadata$weights, c(2, 1))
})

test_that("compute_distances errors on missing block_id column", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(
    compute_distances(left, right, vars = "x", block_id = "nonexistent"),
    "block_id column.*not found"
  )
})

test_that("compute_distances errors on missing left_id column", {
  skip_on_cran()
  left <- data.frame(other_id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(
    compute_distances(left, right, vars = "x", left_id = "id"),
    "left_id column.*not found"
  )
})

test_that("compute_distances errors on missing right_id column", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(other_id = 4:6, x = 4:6)

  expect_error(
    compute_distances(left, right, vars = "x", right_id = "id"),
    "right_id column.*not found"
  )
})

test_that("compute_distances errors on duplicate left IDs", {
  skip_on_cran()
  left <- data.frame(id = c(1, 1, 2), x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(
    compute_distances(left, right, vars = "x"),
    "Duplicate IDs.*left"
  )
})

test_that("compute_distances errors on duplicate right IDs", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = c(4, 4, 5), x = 4:6)

  expect_error(
    compute_distances(left, right, vars = "x"),
    "Duplicate IDs.*right"
  )
})

test_that("compute_distances errors on missing variables", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, y = 4:6)

  expect_error(
    compute_distances(left, right, vars = "x"),
    "Variables not found"
  )
})

test_that("compute_distances errors on non-dataframe inputs", {
  skip_on_cran()
  expect_error(
    compute_distances(1:3, data.frame(id = 1:3, x = 1:3), vars = "x"),
    "must be data frames"
  )

  expect_error(
    compute_distances(data.frame(id = 1:3, x = 1:3), 1:3, vars = "x"),
    "must be data frames"
  )
})

# ------------------------------------------------------------------------------
# is_distance_object tests
# ------------------------------------------------------------------------------

test_that("is_distance_object returns correct values", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  dist_obj <- compute_distances(left, right, vars = "x")

  expect_true(is_distance_object(dist_obj))
  expect_false(is_distance_object(list()))
  expect_false(is_distance_object(NULL))
  expect_false(is_distance_object(data.frame()))
  expect_false(is_distance_object("not a distance object"))
})

# ------------------------------------------------------------------------------
# update_constraints edge cases
# ------------------------------------------------------------------------------

test_that("update_constraints with calipers", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(2, 3, 4, 5, 6))

  dist_obj <- compute_distances(left, right, vars = "x")

  # Apply caliper
  constrained <- update_constraints(dist_obj, calipers = list(x = 1.5))

  expect_true(is_distance_object(constrained))
  expect_true(!is.null(constrained$metadata$constraints_applied))
  expect_equal(constrained$metadata$constraints_applied$calipers, list(x = 1.5))
})

test_that("update_constraints with both max_distance and calipers", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.5, 2.5, 3.5, 4.5, 5.5))

  dist_obj <- compute_distances(left, right, vars = "x")

  # Apply both constraints
  constrained <- update_constraints(dist_obj, max_distance = 2.0, calipers = list(x = 1.0))

  expect_true(is_distance_object(constrained))
  expect_equal(constrained$metadata$constraints_applied$max_distance, 2.0)
  expect_equal(constrained$metadata$constraints_applied$calipers, list(x = 1.0))
})

test_that("update_constraints stores updated_at timestamp", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  dist_obj <- compute_distances(left, right, vars = "x")
  constrained <- update_constraints(dist_obj, max_distance = 1.0)

  expect_true(!is.null(constrained$metadata$constraints_applied$updated_at))
  expect_s3_class(constrained$metadata$constraints_applied$updated_at, "POSIXct")
})

# ------------------------------------------------------------------------------
# print.distance_object edge cases
# ------------------------------------------------------------------------------

test_that("print.distance_object handles weights", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3, y = 1:3)
  right <- data.frame(id = 4:6, x = 4:6, y = 4:6)

  dist_obj <- compute_distances(left, right, vars = c("x", "y"), weights = c(0.5, 1.5))

  expect_output(print(dist_obj), "Weights:")
})

test_that("print.distance_object handles blocking", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3, block = "A")
  right <- data.frame(id = 4:6, x = 4:6, block = "A")

  expect_message(
    dist_obj <- compute_distances(left, right, vars = "x", block_id = "block")
  )

  expect_output(print(dist_obj), "Blocking:")
})

test_that("print.distance_object handles all Inf distances", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 100:102)

  dist_obj <- compute_distances(left, right, vars = "x")

  # Set all to Inf
  dist_obj$cost_matrix[, ] <- Inf

  expect_output(print(dist_obj), "No valid pairs")
})

test_that("print.distance_object shows constraints when applied", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  dist_obj <- compute_distances(left, right, vars = "x")
  constrained <- update_constraints(dist_obj, max_distance = 1.0, calipers = list(x = 0.5))

  expect_output(print(constrained), "Constraints Applied")
  expect_output(print(constrained), "Max distance:")
  expect_output(print(constrained), "Calipers:")
})

# ------------------------------------------------------------------------------
# summary.distance_object edge cases
# ------------------------------------------------------------------------------

test_that("summary.distance_object handles all Inf distances", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 100:102)

  dist_obj <- compute_distances(left, right, vars = "x")

  # Set all to Inf
  dist_obj$cost_matrix[, ] <- Inf

  # Should not error
  expect_output(summary(dist_obj), "Sparsity")
})

test_that("summary.distance_object shows sparsity warning for >50% forbidden", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  dist_obj <- compute_distances(left, right, vars = "x")

  # Set >50% to Inf
  dist_obj$cost_matrix[1:2, ] <- Inf

  expect_output(summary(dist_obj), "method = .lapmod. and .sap.")
})

test_that("summary.distance_object handles highly skewed distribution", {
  skip_on_cran()
  skip_if_not_installed("e1071")

  left <- data.frame(id = 1:10, x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 100))
  right <- data.frame(id = 11:20, x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 100))

  dist_obj <- compute_distances(left, right, vars = "x")

  # The distribution should be highly skewed
  expect_output(summary(dist_obj), "Skewness")
})

test_that("summary.distance_object works without e1071", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  dist_obj <- compute_distances(left, right, vars = "x")

  # Mock missing e1071 by not testing for skewness output
  # Just ensure it doesn't error
  expect_output(summary(dist_obj), "Quantiles")
})

# ------------------------------------------------------------------------------
# Integration tests
# ------------------------------------------------------------------------------

test_that("distance object workflow with match_couples", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  dist_obj <- compute_distances(left, right, vars = "x")

  # Match using distance object
  result <- match_couples(dist_obj)

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 5)
})

test_that("distance object workflow with greedy matching", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  dist_obj <- compute_distances(left, right, vars = "x")

  # Match using distance object with greedy

  result <- match_couples(dist_obj, strategy = "sorted", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 5)
})

test_that("update_constraints then match workflow", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.5, 2.5, 3.5, 4.5, 5.5))

  dist_obj <- compute_distances(left, right, vars = "x")

  # Apply constraints and match - use a less restrictive max_distance
  constrained <- update_constraints(dist_obj, max_distance = 1.0)
  result <- suppressWarnings(match_couples(constrained))

  # Should still match all since distances are 0.5

  expect_equal(nrow(result$pairs), 5)
})

test_that("distance object preserves original data for join_matched", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5, extra = letters[1:5])
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1), extra = letters[6:10])

  dist_obj <- compute_distances(left, right, vars = "x")
  result <- match_couples(dist_obj)

  # join_matched needs the original dataframes from the distance object
  joined <- join_matched(result, dist_obj$original_left, dist_obj$original_right)

  # Overlapping vars get suffix _left and _right
  expect_true("extra_left" %in% names(joined))
  expect_true("extra_right" %in% names(joined))
})
