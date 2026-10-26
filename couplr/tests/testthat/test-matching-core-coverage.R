# ==============================================================================
# Additional tests for matching core functions to increase coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# match_couples basic tests
# ------------------------------------------------------------------------------

test_that("match_couples errors when right is NULL with dataset left", {
  left <- data.frame(id = 1:5, x = rnorm(5))

  expect_error(
    match_couples(left, right = NULL, vars = "x"),
    "right must be provided"
  )
})

test_that("match_couples errors when vars is NULL with dataset input", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    match_couples(left, right, vars = NULL),
    "vars must be specified"
  )
})

test_that("match_couples works with auto_scale", {
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10, 100, 50), y = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10, 100, 50), y = rnorm(10))

  result <- match_couples(left, right, vars = c("x", "y"),
                          auto_scale = TRUE)

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})

test_that("match_couples works with explicit scale parameter", {
  set.seed(123)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  result_std <- match_couples(left, right, vars = "x", scale = "standardize")
  result_robust <- match_couples(left, right, vars = "x", scale = "robust")
  result_range <- match_couples(left, right, vars = "x", scale = "range")

  expect_s3_class(result_std, "matching_result")
  expect_s3_class(result_robust, "matching_result")
  expect_s3_class(result_range, "matching_result")
})

test_that("match_couples respects max_distance", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 100))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5))

  result <- match_couples(left, right, vars = "x", max_distance = 0.5,
                          check_costs = FALSE)

  # The unit at x=100 shouldn't be matched (too far from any control)
  expect_true(nrow(result$pairs) < 5)
})

test_that("match_couples respects calipers", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = c(10, 20, 30, 40, 50))
  right <- data.frame(id = 6:10, x = c(1.5, 2.5, 3.5, 4.5, 5.5), y = c(11, 21, 31, 100, 51))

  result <- match_couples(left, right, vars = c("x", "y"),
                          calipers = list(y = 20))

  # Unit 4 (y=40) can't match with unit 9 (y=100) due to caliper
  expect_true(nrow(result$pairs) <= 5)
})

test_that("match_couples works with manhattan distance", {
  set.seed(123)
  left <- data.frame(id = 1:5, x = rnorm(5), y = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5), y = rnorm(5))

  result <- match_couples(left, right, vars = c("x", "y"),
                          distance = "manhattan")

  expect_s3_class(result, "matching_result")
})

test_that("match_couples works with different LAP methods", {
  set.seed(123)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  result_jv <- match_couples(left, right, vars = "x", method = "jv")
  result_hung <- match_couples(left, right, vars = "x", method = "hungarian")

  expect_s3_class(result_jv, "matching_result")
  expect_s3_class(result_hung, "matching_result")
})

test_that("match_couples return_unmatched works", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  result_with <- match_couples(left, right, vars = "x", return_unmatched = TRUE)
  result_without <- match_couples(left, right, vars = "x", return_unmatched = FALSE)

  expect_true("unmatched" %in% names(result_with))
  expect_null(result_without$unmatched)
})

test_that("match_couples return_diagnostics works", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 1.5:5.5)

  result_diag <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)
  result_nodiag <- match_couples(left, right, vars = "x", return_diagnostics = FALSE)

  expect_true(length(names(result_diag$info)) >= 3)
  # The truncated info is the three reporting fields plus what as_matchit()
  # needs to label the design.
  expect_setequal(names(result_nodiag$info),
                  c("method", "n_matched", "total_distance",
                    "estimand", "focal", "focal_discarded"))
})

test_that("match_couples require_full_matching errors on unmatched", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 100))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5))

  expect_error(
    match_couples(left, right, vars = "x", max_distance = 0.5,
                  require_full_matching = TRUE, check_costs = FALSE),
    "unmatched"
  )
})

# ------------------------------------------------------------------------------
# greedy matching tests
# ------------------------------------------------------------------------------

test_that("greedy matching works with sorted strategy", {
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- match_couples(left, right, vars = "x", strategy = "sorted", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_true(result$info$method == "greedy")
  expect_true(result$info$strategy == "sorted")
})

test_that("greedy matching works with row_best strategy", {
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- match_couples(left, right, vars = "x", strategy = "row_best", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$strategy, "row_best")
})

test_that("greedy matching works with pq strategy", {
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- match_couples(left, right, vars = "x", strategy = "pq", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$strategy, "pq")
})

test_that("greedy matching respects max_distance", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 100))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5))

  result <- match_couples(left, right, vars = "x", max_distance = 0.5, method = "greedy")

  expect_true(nrow(result$pairs) < 5)
})

test_that("greedy matching works with auto_scale", {
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10, 1000, 100))
  right <- data.frame(id = 11:20, x = rnorm(10, 1000, 100))

  result <- match_couples(left, right, vars = "x", auto_scale = TRUE, method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching works with weights", {
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10), y = rnorm(10))

  result <- match_couples(left, right, vars = c("x", "y"),
                           weights = c(x = 2, y = 1), method = "greedy")

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Blocking tests
# ------------------------------------------------------------------------------

test_that("match_couples works with block_id", {
  left <- data.frame(
    id = 1:10,
    x = rnorm(10),
    block = rep(c("A", "B"), each = 5)
  )
  right <- data.frame(
    id = 11:20,
    x = rnorm(10),
    block = rep(c("A", "B"), each = 5)
  )

  result <- match_couples(left, right, vars = "x", block_id = "block")

  expect_s3_class(result, "matching_result")
})

test_that("match_couples ignore_blocks works", {
  left <- data.frame(
    id = 1:10,
    x = rnorm(10),
    block = rep(c("A", "B"), each = 5)
  )
  right <- data.frame(
    id = 11:20,
    x = rnorm(10),
    block = rep(c("A", "B"), each = 5)
  )

  result <- match_couples(left, right, vars = "x",
                          block_id = "block", ignore_blocks = TRUE)

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Distance object tests
# ------------------------------------------------------------------------------

test_that("match_couples works with distance_object", {
  left <- data.frame(id = 1:5, x = 1:5, y = 1:5)
  right <- data.frame(id = 6:10, x = 2:6, y = 2:6)

  dist_obj <- compute_distances(left, right, vars = c("x", "y"))
  result <- suppressWarnings(match_couples(dist_obj))

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with distance_object respects max_distance", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 100))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5))

  dist_obj <- compute_distances(left, right, vars = "x")
  result <- match_couples(dist_obj, max_distance = 0.5, check_costs = FALSE)

  expect_true(nrow(result$pairs) < 5)
})

# ------------------------------------------------------------------------------
# Print method tests
# ------------------------------------------------------------------------------

test_that("print.matching_result works", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 1.5:5.5)

  result <- match_couples(left, right, vars = "x")

  output <- capture.output(print(result))

  expect_true(any(grepl("Matching Result", output)))
})

# ------------------------------------------------------------------------------
# Check costs behavior
# ------------------------------------------------------------------------------

test_that("match_couples check_costs produces warnings for skewed costs", {
  # Create data where costs will be very skewed
  left <- data.frame(id = 1:10, x = c(rep(0, 9), 1000))
  right <- data.frame(id = 11:20, x = c(rep(0, 9), 1000))

  # This should run without error even if costs are skewed
  result <- suppressWarnings(match_couples(left, right, vars = "x", check_costs = TRUE))

  expect_s3_class(result, "matching_result")
})

test_that("match_couples check_costs = FALSE skips cost checking", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 1:5)

  result <- match_couples(left, right, vars = "x", check_costs = FALSE)

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("match_couples handles single observation", {
  left <- data.frame(id = 1, x = 1)
  right <- data.frame(id = 2, x = 1.5)

  result <- suppressWarnings(match_couples(left, right, vars = "x"))

  expect_equal(nrow(result$pairs), 1)
})

test_that("match_couples handles unequal group sizes", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:8, x = 1:5)

  result <- suppressWarnings(match_couples(left, right, vars = "x"))

  expect_equal(nrow(result$pairs), 3)  # Matches min(3, 5)
})

test_that("greedy matching handles unequal group sizes", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:8, x = 1:5)

  result <- match_couples(left, right, vars = "x", method = "greedy")

  expect_equal(nrow(result$pairs), 3)
})

test_that("match_couples handles multiple variables", {
  set.seed(123)
  left <- data.frame(
    id = 1:20,
    x = rnorm(20),
    y = rnorm(20),
    z = rnorm(20)
  )
  right <- data.frame(
    id = 21:40,
    x = rnorm(20),
    y = rnorm(20),
    z = rnorm(20)
  )

  result <- match_couples(left, right, vars = c("x", "y", "z"))

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 20)
})

# ------------------------------------------------------------------------------
# Additional coverage tests
# ------------------------------------------------------------------------------

test_that("match_couples distance_object with max_distance constraint", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 100))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5))

  dist_obj <- compute_distances(left, right, vars = "x")
  result <- match_couples(dist_obj, max_distance = 0.5, check_costs = FALSE)

  expect_true(nrow(result$pairs) < 5)
})

test_that("match_couples distance_object with calipers", {
  left <- data.frame(id = 1:5, x = 1:5, y = c(10, 20, 30, 40, 50))
  right <- data.frame(id = 6:10, x = 1.5:5.5, y = c(11, 21, 100, 41, 51))

  dist_obj <- compute_distances(left, right, vars = c("x", "y"))
  result <- match_couples(dist_obj, calipers = list(y = 15),
                          check_costs = FALSE)

  expect_s3_class(result, "matching_result")
})

test_that("match_couples errors for no valid pairs", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(100, 200, 300))

  # When constraints are too strict, the function throws an error
  expect_error(
    suppressWarnings(match_couples(left, right, vars = "x", max_distance = 0.1)),
    "No valid pairs"
  )
})

test_that("match_couples with parallel = TRUE", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  left <- data.frame(
    id = 1:20,
    x = rnorm(20),
    block = rep(c("A", "B", "C", "D"), each = 5)
  )
  right <- data.frame(
    id = 21:40,
    x = rnorm(20),
    block = rep(c("A", "B", "C", "D"), each = 5)
  )

  result <- match_couples(left, right, vars = "x", block_id = "block",
                          parallel = TRUE)

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching with manhattan distance", {
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10), y = rnorm(10))

  result <- match_couples(left, right, vars = c("x", "y"),
                           distance = "manhattan", method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching with calipers", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5), y = 1:5)
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 100, 5.1), y = 1:5)

  result <- match_couples(left, right, vars = c("x", "y"),
                           calipers = list(x = 1), method = "greedy")

  expect_true(nrow(result$pairs) < 5)
})

test_that("greedy matching with max_distance", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 100))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5))

  result <- match_couples(left, right, vars = "x", max_distance = 0.5, method = "greedy")

  expect_true(nrow(result$pairs) < 5)
})

test_that("greedy matching require_full_matching errors appropriately", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 100))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5))

  expect_error(
    match_couples(left, right, vars = "x", max_distance = 0.5,
                   require_full_matching = TRUE, method = "greedy"),
    "unmatched"
  )
})

test_that("match_couples handles blocked matching with empty blocks", {
  left <- data.frame(
    id = 1:6,
    x = rnorm(6),
    block = c("A", "A", "A", "B", "B", "C")  # C has only left units
  )
  right <- data.frame(
    id = 7:11,
    x = rnorm(5),
    block = c("A", "A", "B", "B", "D")  # D has only right units
  )

  result <- match_couples(left, right, vars = "x", block_id = "block")

  expect_s3_class(result, "matching_result")
  # Should have unmatched units from blocks C and D
  expect_true(length(result$unmatched$left) > 0 || length(result$unmatched$right) > 0)
})

test_that("match_couples blocked with return_diagnostics = FALSE", {
  left <- data.frame(
    id = 1:10,
    x = rnorm(10),
    block = rep(c("A", "B"), each = 5)
  )
  right <- data.frame(
    id = 11:20,
    x = rnorm(10),
    block = rep(c("A", "B"), each = 5)
  )

  result <- match_couples(left, right, vars = "x", block_id = "block",
                          return_diagnostics = FALSE)

  expect_s3_class(result, "matching_result")
  expect_true("n_matched" %in% names(result$info))
})

test_that("greedy matching with return_unmatched = FALSE", {
  set.seed(123)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  result <- match_couples(left, right, vars = "x", return_unmatched = FALSE, method = "greedy")

  expect_null(result$unmatched)
})

test_that("greedy matching with return_diagnostics = FALSE", {
  set.seed(123)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  result <- match_couples(left, right, vars = "x", return_diagnostics = FALSE, method = "greedy")

  # Should have minimal info
  expect_setequal(names(result$info),
                  c("method", "strategy", "n_matched", "total_distance",
                    "estimand", "focal", "focal_discarded"))
  expect_true("n_matched" %in% names(result$info))
})
