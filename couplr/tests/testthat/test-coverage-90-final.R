# Tests targeting 90%+ coverage - uncovered branches

# ==============================================================================
# matching_core.R - Error handling branches
# ==============================================================================

test_that("match_couples validates NULL right", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = rnorm(3))
  expect_error(match_couples(left, right = NULL, vars = "x"))
})

test_that("match_couples validates NULL vars", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = rnorm(3))
  right <- data.frame(id = 4:6, x = rnorm(3))
  expect_error(match_couples(left, right, vars = NULL))
})

test_that("greedy matching validates NULL right", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = rnorm(3))
  expect_error(match_couples(left, right = NULL, vars = "x", method = "greedy"))
})

test_that("greedy matching validates NULL vars", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = rnorm(3))
  right <- data.frame(id = 4:6, x = rnorm(3))
  expect_error(match_couples(left, right, vars = NULL, method = "greedy"))
})

test_that("greedy matching handles sparse matching", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(100, 200, 300))
  expect_warning(
    match_couples(left, right, vars = "x", max_distance = 0.001, auto_scale = FALSE, method = "greedy")
  )
})

# ==============================================================================
# matching_preprocessing.R - Edge cases
# ==============================================================================

test_that("preprocess_matching_vars handles empty vars", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = rnorm(3))
  right <- data.frame(id = 4:6, x = rnorm(3))
  expect_error(preprocess_matching_vars(left, right, vars = character(0)))
})

test_that("preprocess_matching_vars detects constant variable", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = rep(5, 5), y = 1:5)
  right <- data.frame(id = 6:10, x = rep(5, 5), y = 6:10)
  expect_warning(preprocess_matching_vars(left, right, vars = c("x", "y")))
})

test_that("preprocess_matching_vars with different scale options", {
  skip_on_cran()
  left <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10), y = rnorm(10))
  for (scale in c("robust", "standardize", "range", "none")) {
    result <- preprocess_matching_vars(left, right, vars = c("x", "y"), scale = scale)
    expect_true(!is.null(result))
  }
})

# ==============================================================================
# matching_diagnostics.R - Untested code paths
# ==============================================================================

test_that("balance_diagnostics handles single observation", {
  skip_on_cran()
  left <- data.frame(id = 1, x = 5)
  right <- data.frame(id = 2, x = 5.1)
  suppressWarnings(result <- match_couples(left, right, vars = "x"))
  diag <- balance_diagnostics(result, left, right, vars = "x")
  expect_s3_class(diag, "balance_diagnostics")
})

test_that("balance_diagnostics print works", {
  skip_on_cran()
  left <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10), y = rnorm(10))
  result <- match_couples(left, right, vars = c("x", "y"))
  diag <- balance_diagnostics(result, left, right, vars = c("x", "y"))
  expect_no_error(print(diag))
})

# ==============================================================================
# lap_solve.R - Validation and special cases
# ==============================================================================

test_that("lap_solve handles 1x1 matrix", {
  skip_on_cran()
  cost <- matrix(42, nrow = 1, ncol = 1)
  result <- lap_solve(cost)
  expect_equal(sum(result$cost), 42)
})

test_that("lap_solve auto-selects for binary costs", {
  skip_on_cran()
  cost <- matrix(c(0, 1, 1, 0), nrow = 2)
  result <- lap_solve(cost, method = "auto")
  expect_equal(nrow(result), 2)
})

test_that("lap_solve_line_metric validates empty input", {
  skip_on_cran()
  expect_error(lap_solve_line_metric(numeric(0), numeric(0)))
})

test_that("lap_solve_line_metric validates infinite values", {
  skip_on_cran()
  expect_error(lap_solve_line_metric(c(1, Inf), c(2, 3)))
})

test_that("bottleneck_assignment handles square matrix", {
  skip_on_cran()
  cost <- matrix(1:9, nrow = 3, ncol = 3)

  result <- bottleneck_assignment(cost)
  expect_true("bottleneck" %in% names(result))
  expect_equal(result$status, "optimal")
})

test_that("sinkhorn validates lambda", {
  skip_on_cran()
  cost <- matrix(1:4, nrow = 2)
  expect_error(sinkhorn(cost, lambda = -1))
  expect_error(sinkhorn(cost, lambda = 0))
})

# ==============================================================================
# morph_pixel.R - Boundary conditions
# ==============================================================================

test_that("pixel_morph warns on invalid upscale", {
  skip_on_cran()
  skip_if_not_installed("png")
  skip_if_not_installed("magick")
  img <- magick::image_blank(8, 8, "white")
  expect_warning(pixel_morph(img, img, upscale = 0))
  expect_warning(pixel_morph(img, img, upscale = -1))
})

test_that("pixel_morph warns on invalid n_frames", {
  skip_on_cran()
  skip_if_not_installed("png")
  skip_if_not_installed("magick")
  img <- magick::image_blank(8, 8, "white")
  expect_warning(pixel_morph(img, img, n_frames = 0))
  expect_warning(pixel_morph(img, img, n_frames = 1))
})

# ==============================================================================
# matching_distance_cache.R - Edge cases
# ==============================================================================

test_that("compute_distances validates ID columns", {
  skip_on_cran()
  left <- data.frame(x = 1:3)
  right <- data.frame(x = 4:6)
  expect_error(compute_distances(left, right, vars = "x"))
})

test_that("compute_distances detects duplicate IDs", {
  skip_on_cran()
  left <- data.frame(id = c(1, 1, 2), x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  expect_error(compute_distances(left, right, vars = "x"))
})

# ==============================================================================
# matching_blocks.R - Edge cases
# ==============================================================================

test_that("matchmaker handles 'none' block_type", {
  skip_on_cran()
  left <- data.frame(id = 1:5, group = c("A", "A", "B", "B", "B"))
  right <- data.frame(id = 6:10, group = c("A", "B", "B", "A", "B"))
  blocks <- matchmaker(left, right, block_type = "none")
  expect_true(inherits(blocks, "matchmaker_result") || inherits(blocks, "data.frame"))
})

test_that("matchmaker validates block_type", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  expect_error(matchmaker(left, right, block_type = "invalid"))
})

# ==============================================================================
# matching_constraints.R - Edge cases
# ==============================================================================

test_that("update_constraints rejects max_distance = 0, consistent with apply_max_distance()", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1, 2, 3))
  dist_obj <- compute_distances(left, right, vars = "x")
  expect_error(update_constraints(dist_obj, max_distance = 0), "positive")
})

# ==============================================================================
# Additional edge cases
# ==============================================================================

test_that("assignment handles all-Inf matrix", {
  skip_on_cran()
  cost <- matrix(Inf, nrow = 3, ncol = 3)
  expect_error(assignment(cost))
})

test_that("assignment handles mixed Inf matrix", {
  skip_on_cran()
  cost <- matrix(c(1, Inf, Inf, Inf, 2, Inf, Inf, Inf, 3), nrow = 3)
  result <- assignment(cost)
  expect_equal(result$status, "optimal")
  expect_equal(result$total_cost, 6)
})

test_that("assignment with maximize = TRUE", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- assignment(cost, maximize = TRUE)
  expect_equal(result$status, "optimal")
})

test_that("assignment with different methods", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
  for (method in c("hungarian", "jv", "auction")) {
    result <- assignment(cost, method = method)
    expect_equal(result$status, "optimal")
  }
})

test_that("assignment_duals returns dual variables", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
  result <- assignment_duals(cost)
  expect_true("u" %in% names(result))
  expect_true("v" %in% names(result))
})

# ==============================================================================
# Augment and join coverage
# ==============================================================================

test_that("augment.matching_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")
  augmented <- augment(result, left, right)
  expect_true(nrow(augmented) > 0)
})

test_that("join_matched works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")
  joined <- join_matched(result, left, right)
  expect_true(nrow(joined) > 0 || nrow(result$pairs) == 0)
})

# ==============================================================================
# Edge cases in core matching
# ==============================================================================

test_that("match_couples with caliper", {
  skip_on_cran()
  left <- data.frame(id = 1:10, x = 1:10, y = rnorm(10))
  right <- data.frame(id = 11:20, x = 1:10 + 0.5, y = rnorm(10))
  result <- match_couples(left, right, vars = c("x", "y"), caliper = c(x = 1),
                          check_costs = FALSE)
  expect_s3_class(result, "matching_result")
})

test_that("match_couples with weight parameter", {
  skip_on_cran()
  left <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10), y = rnorm(10))
  result <- match_couples(left, right, vars = c("x", "y"), weights = c(x = 2, y = 1))
  expect_s3_class(result, "matching_result")
})

test_that("match_couples with blocking", {
  skip_on_cran()
  left <- data.frame(id = 1:20, block = rep(1:4, 5), x = rnorm(20))
  right <- data.frame(id = 21:40, block = rep(1:4, 5), x = rnorm(20))
  result <- match_couples(left, right, vars = "x", block_id = "block")
  expect_s3_class(result, "matching_result")
})

test_that("match_couples return_diagnostics", {
  skip_on_cran()
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)
  # return_diagnostics keeps additional fields in info like solver
  expect_true("solver" %in% names(result$info) || length(result$info) > 3)
})

# ==============================================================================
# Greedy strategies
# ==============================================================================

test_that("greedy matching with different strategies", {
  skip_on_cran()
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  for (strategy in c("sorted", "row_best", "pq")) {
    result <- match_couples(left, right, vars = "x", strategy = strategy, method = "greedy")
    expect_s3_class(result, "matching_result")
  }
})

# ==============================================================================
# K-best and batch
# ==============================================================================

test_that("lap_solve_kbest works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3)
  result <- lap_solve_kbest(cost, k = 2)
  expect_true(nrow(result) >= 1)
})

test_that("lap_solve_batch works", {
  skip_on_cran()
  costs <- list(
    matrix(1:4, nrow = 2),
    matrix(1:9, nrow = 3)
  )
  result <- lap_solve_batch(costs)
  # Returns a tibble with columns: problem_id, source, target, cost, total_cost, method_used
  expect_true(inherits(result, "data.frame"))
  expect_true("problem_id" %in% names(result))
  # 2 problems: 2x2 (2 matches) + 3x3 (3 matches) = 5 rows
  expect_true(nrow(result) >= 5)
})

# ==============================================================================
# Print methods
# ==============================================================================

test_that("print.matching_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")
  expect_no_error(print(result))
})

test_that("summary.matching_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")
  expect_no_error(summary(result))
})
