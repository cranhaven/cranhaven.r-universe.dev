# ==============================================================================
# Extended tests for matching_core.R - covering untested paths
# ==============================================================================

# ------------------------------------------------------------------------------
# match_couples error cases
# ------------------------------------------------------------------------------

test_that("match_couples errors when right is NULL and left is not distance object", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))

  expect_error(
    match_couples(left, right = NULL, vars = "x"),
    "right must be provided"
  )
})

test_that("match_couples errors when vars is NULL with datasets", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  expect_error(
    match_couples(left, right, vars = NULL),
    "vars must be specified"
  )
})

# ------------------------------------------------------------------------------
# match_couples with distance object
# ------------------------------------------------------------------------------

test_that("match_couples works with distance object", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3), y = c(2, 4, 6))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1), y = c(2.1, 4.1, 6.1))

  # Create distance object
  dist_obj <- compute_distances(left, right, vars = c("x", "y"))

  # Match using distance object
  result <- match_couples(dist_obj)

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$n_matched, 3)
})

test_that("match_couples from distance object with max_distance constraint errors", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3), y = c(2, 4, 6))
  right <- data.frame(id = 4:6, x = c(10, 20, 30), y = c(20, 40, 60))

  dist_obj <- compute_distances(left, right, vars = c("x", "y"))

  # With very restrictive max_distance - errors because no valid pairs
  expect_error(
    suppressWarnings(match_couples(dist_obj, max_distance = 0.1)),
    "No valid pairs"
  )
})

test_that("match_couples handles check_costs = FALSE", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  # Should not produce warnings when check_costs = FALSE
  result <- match_couples(left, right, vars = "x", check_costs = FALSE)

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# match_couples with auto_scale
# ------------------------------------------------------------------------------

test_that("match_couples auto_scale works", {
  skip_on_cran()
  set.seed(123)
  left <- data.frame(id = 1:10, x = rnorm(10, 100, 10), y = rnorm(10, 1000, 100))
  right <- data.frame(id = 11:20, x = rnorm(10, 100, 10), y = rnorm(10, 1000, 100))

  result <- match_couples(left, right, vars = c("x", "y"), auto_scale = TRUE)

  expect_s3_class(result, "matching_result")
  expect_true(result$info$n_matched > 0)
})

# ------------------------------------------------------------------------------
# match_couples with return_diagnostics and return_unmatched FALSE
# ------------------------------------------------------------------------------

test_that("match_couples return_diagnostics FALSE strips info", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x", return_diagnostics = FALSE)

  expect_true("method" %in% names(result$info))
  expect_true("n_matched" %in% names(result$info))
  expect_true("total_distance" %in% names(result$info))
  # More detailed info should be stripped
  expect_false("scaled" %in% names(result$info))
})

test_that("match_couples return_unmatched FALSE removes unmatched", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:8, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x", return_unmatched = FALSE)

  expect_null(result$unmatched)
})

# ------------------------------------------------------------------------------
# match_couples with require_full_matching
# ------------------------------------------------------------------------------

test_that("match_couples require_full_matching errors when not achieved", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 100, 200))
  right <- data.frame(id = 6:8, x = c(1.1, 2.1, 3.1))

  # Very tight constraint will leave units unmatched
  expect_error(
    match_couples(left, right, vars = "x",
                  max_distance = 1,
                  require_full_matching = TRUE,
                  check_costs = FALSE),
    "Full matching required"
  )
})

# ------------------------------------------------------------------------------
# Blocking tests
# ------------------------------------------------------------------------------

test_that("match_couples with explicit block_id", {
  skip_on_cran()
  left <- data.frame(
    id = 1:6,
    x = c(1, 2, 3, 10, 11, 12),
    block = c("A", "A", "A", "B", "B", "B")
  )
  right <- data.frame(
    id = 7:12,
    x = c(1.1, 2.1, 3.1, 10.1, 11.1, 12.1),
    block = c("A", "A", "A", "B", "B", "B")
  )

  result <- match_couples(left, right, vars = "x", block_id = "block",
                          return_diagnostics = TRUE)

  expect_s3_class(result, "matching_result")
  expect_true("block_id" %in% names(result$pairs))
  # Block info is in block_summary
  expect_true(!is.null(result$info$block_summary))
})

test_that("detect_blocking errors when block_id not in both datasets", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3, my_block = c("A", "A", "B"))
  right <- data.frame(id = 4:6, x = 4:6)  # No block column

  expect_error(
    match_couples(left, right, vars = "x", block_id = "my_block"),
    "not found in right"
  )
})

test_that("match_couples with ignore_blocks = TRUE", {
  skip_on_cran()
  left <- data.frame(
    id = 1:4,
    x = c(1, 2, 10, 11),
    block_id = c("A", "A", "B", "B")
  )
  right <- data.frame(
    id = 5:8,
    x = c(1.1, 2.1, 10.1, 11.1),
    block_id = c("A", "A", "B", "B")
  )

  result <- match_couples(left, right, vars = "x", ignore_blocks = TRUE)

  expect_s3_class(result, "matching_result")
  # No block_id column in output when ignoring blocks
  expect_false("block_id" %in% names(result$pairs))
})

# ------------------------------------------------------------------------------
# Blocked matching with empty blocks
# ------------------------------------------------------------------------------

test_that("blocked matching handles one-sided blocks", {
  skip_on_cran()
  left <- data.frame(
    id = 1:4,
    x = c(1, 2, 10, 11),
    block_id = c("A", "A", "B", "B")
  )
  right <- data.frame(
    id = 5:6,
    x = c(1.1, 2.1),
    block_id = c("A", "A")  # No B block
  )

  result <- match_couples(left, right, vars = "x", block_id = "block_id")

  expect_s3_class(result, "matching_result")
  # Left units in block B should be unmatched
  expect_true(length(result$unmatched$left) >= 2)
})

# ------------------------------------------------------------------------------
# greedy matching tests
# ------------------------------------------------------------------------------

test_that("greedy matching basic functionality", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$method, "greedy")
  expect_equal(result$info$strategy, "row_best")
})

test_that("greedy matching with sorted strategy", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x", strategy = "sorted", method = "greedy")

  expect_equal(result$info$strategy, "sorted")
})

test_that("greedy matching with pq strategy", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x", strategy = "pq", method = "greedy")

  expect_equal(result$info$strategy, "pq")
})

test_that("greedy matching errors when right is NULL", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)

  expect_error(
    match_couples(left, right = NULL, vars = "x", method = "greedy"),
    "right must be provided"
  )
})

test_that("greedy matching errors when vars is NULL", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)

  expect_error(
    match_couples(left, right, vars = NULL, method = "greedy"),
    "vars must be specified"
  )
})

test_that("greedy matching with auto_scale", {
  skip_on_cran()
  set.seed(456)
  left <- data.frame(id = 1:10, x = rnorm(10, 100, 10))
  right <- data.frame(id = 11:20, x = rnorm(10, 100, 10))

  result <- match_couples(left, right, vars = "x", auto_scale = TRUE, method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching with blocking", {
  skip_on_cran()
  left <- data.frame(
    id = 1:6,
    x = c(1, 2, 3, 10, 11, 12),
    block_id = c("A", "A", "A", "B", "B", "B")
  )
  right <- data.frame(
    id = 7:12,
    x = c(1.1, 2.1, 3.1, 10.1, 11.1, 12.1),
    block_id = c("A", "A", "A", "B", "B", "B")
  )

  result <- match_couples(left, right, vars = "x", block_id = "block_id", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_true("block_id" %in% names(result$pairs))
})

test_that("greedy matching with require_full_matching", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 100, 200))
  right <- data.frame(id = 6:8, x = c(1.1, 2.1, 3.1))

  expect_error(
    match_couples(left, right, vars = "x",
                   max_distance = 1,
                   require_full_matching = TRUE, method = "greedy"),
    "Full matching required"
  )
})

test_that("greedy matching return_diagnostics FALSE", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x", return_diagnostics = FALSE, method = "greedy")

  # Only essential fields
  expect_true("method" %in% names(result$info))
  expect_true("strategy" %in% names(result$info))
  expect_false("scaled" %in% names(result$info))
})

test_that("greedy matching from distance object", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  dist_obj <- compute_distances(left, right, vars = "x")

  result <- match_couples(dist_obj, method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$method, "greedy")
})

test_that("greedy matching from distance object with no valid pairs", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(100, 200, 300))

  dist_obj <- compute_distances(left, right, vars = "x")

  # Very tight constraint
  expect_warning(
    result <- match_couples(dist_obj, max_distance = 0.001, method = "greedy"),
    "No valid pairs"
  )

  expect_equal(result$info$n_matched, 0)
})

# ------------------------------------------------------------------------------
# Print/Summary/Plot methods for matching_result
# ------------------------------------------------------------------------------

test_that("print.matching_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  output <- capture.output(print(result))

  expect_true(any(grepl("Matching Result", output)))
  expect_true(any(grepl("Pairs matched", output)))
})

test_that("print.matching_result with strategy", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x", strategy = "sorted", method = "greedy")

  output <- capture.output(print(result))

  expect_true(any(grepl("Strategy", output)))
})

test_that("print.matching_result with blocks", {
  skip_on_cran()
  left <- data.frame(
    id = 1:4,
    x = c(1, 2, 10, 11),
    block_id = c("A", "A", "B", "B")
  )
  right <- data.frame(
    id = 5:8,
    x = c(1.1, 2.1, 10.1, 11.1),
    block_id = c("A", "A", "B", "B")
  )

  result <- match_couples(left, right, vars = "x", block_id = "block_id",
                          return_diagnostics = TRUE)

  output <- capture.output(print(result))

  # Should show block_id in paired output
  expect_true(any(grepl("block_id", output)))
})

test_that("summary.matching_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x")
  smry <- summary(result)

  expect_s3_class(smry, "summary.matching_result")
  expect_equal(smry$n_matched, 5)
  expect_true(!is.na(smry$mean_distance))
})

test_that("print.summary.matching_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")
  smry <- summary(result)

  output <- capture.output(print(smry))

  expect_true(any(grepl("Summary", output)))
})

test_that("print.summary.matching_result with strategy", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x", method = "greedy")
  smry <- summary(result)

  output <- capture.output(print(smry))

  expect_true(any(grepl("row_best", output)))
})

test_that("plot.matching_result histogram", {
  skip_on_cran()
  skip_if_not_installed("graphics")

  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x")

  # Should not error
  expect_silent(plot(result, type = "histogram"))
})

test_that("plot.matching_result density", {
  skip_on_cran()
  skip_if_not_installed("graphics")

  # Need more data points for density estimation
  left <- data.frame(id = 1:50, x = 1:50)
  right <- data.frame(id = 51:100, x = seq(1.1, 50.1, by = 1))

  result <- suppressWarnings(match_couples(left, right, vars = "x"))

  # May produce tie warnings, just check it works
  expect_error(suppressWarnings(plot(result, type = "density")), NA)
})

test_that("plot.matching_result ecdf", {
  skip_on_cran()
  skip_if_not_installed("graphics")

  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x")

  expect_silent(plot(result, type = "ecdf"))
})

test_that("plot.matching_result handles empty result", {
  skip_on_cran()
  # Create a synthetic result with no matches
  result <- structure(
    list(
      pairs = tibble::tibble(
        left_id = character(0),
        right_id = character(0),
        distance = numeric(0)
      ),
      unmatched = list(left = c("1", "2", "3"), right = c("4", "5", "6")),
      info = list(
        method = "lap",
        n_matched = 0,
        total_distance = 0
      )
    ),
    class = c("matching_result", "couplr_result")
  )

  expect_message(plot(result), "No matched pairs to plot")
})
