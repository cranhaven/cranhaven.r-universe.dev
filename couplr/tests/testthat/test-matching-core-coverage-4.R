# ==============================================================================
# Additional tests for matching_core.R to reach 90%+ coverage
# ==============================================================================

# Create test datasets
set.seed(123)
left_df <- data.frame(
  id = paste0("L", 1:20),
  age = rnorm(20, 40, 10),
  income = rnorm(20, 50000, 15000),
  stringsAsFactors = FALSE
)
right_df <- data.frame(
 id = paste0("R", 1:20),
  age = rnorm(20, 42, 10),
  income = rnorm(20, 52000, 15000),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Error handling tests
# ------------------------------------------------------------------------------

test_that("match_couples errors when right is NULL (non-distance object)", {
  skip_on_cran()
  expect_error(
    match_couples(left_df, right = NULL, vars = c("age")),
    "right must be provided"
  )
})

test_that("match_couples errors when vars is NULL (non-distance object)", {
  skip_on_cran()
  expect_error(
    match_couples(left_df, right_df, vars = NULL),
    "vars must be specified"
  )
})

# ------------------------------------------------------------------------------
# auto_scale path
# ------------------------------------------------------------------------------

test_that("match_couples with auto_scale=TRUE works", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income"),
    auto_scale = TRUE
  )

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})

test_that("match_couples with auto_scale and explicit scale", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income"),
    auto_scale = TRUE,
    scale = "robust"
  )

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# require_full_matching path
# ------------------------------------------------------------------------------

test_that("match_couples with require_full_matching=TRUE succeeds with balanced data", {
  skip_on_cran()
  left_small <- left_df[1:5, ]
  right_small <- right_df[1:5, ]

  result <- match_couples(
    left_small, right_small,
    vars = c("age"),
    require_full_matching = TRUE
  )

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 5)
})

test_that("match_couples with require_full_matching=TRUE errors on unmatched", {
  skip_on_cran()
  left_small <- left_df[1:5, ]
  right_small <- right_df[1:3, ]

  expect_error(
    match_couples(
      left_small, right_small,
      vars = c("age"),
      require_full_matching = TRUE
    ),
    "Full matching required"
  )
})

# ------------------------------------------------------------------------------
# return_unmatched and return_diagnostics paths
# ------------------------------------------------------------------------------

test_that("match_couples with return_unmatched=FALSE removes unmatched", {
  skip_on_cran()
  result <- match_couples(
    left_df[1:5, ], right_df[1:3, ],
    vars = c("age"),
    return_unmatched = FALSE
  )

  expect_null(result$unmatched)
})

test_that("match_couples with return_diagnostics=FALSE limits info", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age"),
    return_diagnostics = FALSE
  )

  expect_true("method" %in% names(result$info))
  expect_true("n_matched" %in% names(result$info))
  expect_true("total_distance" %in% names(result$info))
})

test_that("match_couples with return_diagnostics=TRUE has full info", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age"),
    return_diagnostics = TRUE
  )

  expect_true(length(names(result$info)) > 3)
})

# ------------------------------------------------------------------------------
# blocking paths
# ------------------------------------------------------------------------------

test_that("match_couples with explicit block_id works", {
  skip_on_cran()
  left_blocked <- left_df
  left_blocked$block <- rep(c("A", "B"), each = 10)
  right_blocked <- right_df
  right_blocked$block <- rep(c("A", "B"), each = 10)

  result <- match_couples(
    left_blocked, right_blocked,
    vars = c("age"),
    block_id = "block"
  )

  expect_s3_class(result, "matching_result")
  expect_true("block_id" %in% names(result$pairs))
})

test_that("match_couples errors when block_id not in left", {
  skip_on_cran()
  left_blocked <- left_df
  right_blocked <- right_df
  right_blocked$block <- rep(c("A", "B"), each = 10)

  expect_error(
    match_couples(
      left_blocked, right_blocked,
      vars = c("age"),
      block_id = "block"
    ),
    "not found in left"
  )
})

test_that("match_couples errors when block_id not in right", {
  skip_on_cran()
  left_blocked <- left_df
  left_blocked$block <- rep(c("A", "B"), each = 10)
  right_blocked <- right_df

  expect_error(
    match_couples(
      left_blocked, right_blocked,
      vars = c("age"),
      block_id = "block"
    ),
    "not found in right"
  )
})

test_that("match_couples with ignore_blocks=TRUE ignores blocks", {
  skip_on_cran()
  left_blocked <- left_df
  left_blocked$block_id <- rep(c("A", "B"), each = 10)
  right_blocked <- right_df
  right_blocked$block_id <- rep(c("A", "B"), each = 10)

  result <- match_couples(
    left_blocked, right_blocked,
    vars = c("age"),
    ignore_blocks = TRUE
  )

  expect_s3_class(result, "matching_result")
  # Without blocking, pairs should not have block_id column
  expect_false("block_id" %in% names(result$pairs))
})

# ------------------------------------------------------------------------------
# blocked matching with empty blocks
# ------------------------------------------------------------------------------

test_that("blocked matching handles empty blocks", {
  skip_on_cran()
  left_blocked <- left_df[1:10, ]
  left_blocked$block <- c(rep("A", 5), rep("B", 5))
  right_blocked <- right_df[1:8, ]
  right_blocked$block <- c(rep("A", 5), rep("C", 3))  # No B in right

  result <- match_couples(
    left_blocked, right_blocked,
    vars = c("age"),
    block_id = "block"
  )

  expect_s3_class(result, "matching_result")
  # Block B should have unmatched left units
  expect_true(length(result$unmatched$left) > 0)
})

# ------------------------------------------------------------------------------
# check_costs path
# ------------------------------------------------------------------------------

test_that("match_couples with check_costs=FALSE skips cost check", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age"),
    check_costs = FALSE
  )

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# greedy matching tests
# ------------------------------------------------------------------------------

test_that("greedy matching works with default strategy", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income")
  , method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})

test_that("greedy matching with strategy='sorted' works", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age"),
    strategy = "sorted"
  , method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching with strategy='pq' works", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age"),
    strategy = "pq"
  , method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching with auto_scale works", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income"),
    auto_scale = TRUE
  , method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching with max_distance constraint", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age"),
    max_distance = 5
  , method = "greedy")

  expect_s3_class(result, "matching_result")
  # Some pairs may be excluded due to distance constraint
})

test_that("greedy matching with calipers", {
  skip_on_cran()
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income"),
    calipers = list(age = 10)
  , method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy matching with blocking", {
  skip_on_cran()
  left_blocked <- left_df
  left_blocked$block <- rep(c("A", "B"), each = 10)
  right_blocked <- right_df
  right_blocked$block <- rep(c("A", "B"), each = 10)

  result <- match_couples(
    left_blocked, right_blocked,
    vars = c("age"),
    block_id = "block"
  , method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_true("block_id" %in% names(result$pairs))
})

# ------------------------------------------------------------------------------
# distance_object path
# ------------------------------------------------------------------------------

test_that("match_couples with distance_object works", {
  skip_on_cran()
  dist_obj <- compute_distances(
    left_df, right_df,
    vars = c("age", "income"),
    distance = "euclidean",
    scale = "standardize"
  )

  result <- match_couples(dist_obj)

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})

test_that("match_couples with distance_object and max_distance", {
  skip_on_cran()
  dist_obj <- compute_distances(
    left_df, right_df,
    vars = c("age"),
    distance = "euclidean"
  )

  # max_distance = 25 leaves enough valid edges for a feasible matching on
  # this seeded data; smaller thresholds make at least one row edge-free.
  result <- match_couples(dist_obj, max_distance = 25)

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with distance_object and calipers", {
  skip_on_cran()
  dist_obj <- compute_distances(
    left_df, right_df,
    vars = c("age", "income"),
    distance = "euclidean"
  )

  result <- match_couples(dist_obj, calipers = list(age = 10))

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with distance_object and require_full_matching", {
  skip_on_cran()
  dist_obj <- compute_distances(
    left_df[1:5, ], right_df[1:5, ],
    vars = c("age"),
    distance = "euclidean"
  )

  result <- match_couples(dist_obj, require_full_matching = TRUE)

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 5)
})

# ------------------------------------------------------------------------------
# Edge cases: no valid pairs
# ------------------------------------------------------------------------------

test_that("match_couples errors when no valid pairs exist", {
  skip_on_cran()
  # Create datasets with very different values that will all be forbidden
  left_small <- data.frame(id = "L1", age = 0)
  right_small <- data.frame(id = "R1", age = 1000)

  expect_error(
    suppressWarnings(match_couples(
      left_small, right_small,
      vars = "age",
      max_distance = 0.001  # Very tight constraint
    )),
    "No valid pairs"
  )
})

# ------------------------------------------------------------------------------
# Multiple LAP methods
# ------------------------------------------------------------------------------

test_that("match_couples with method='hungarian' works", {
  skip_on_cran()
  result <- match_couples(
    left_df[1:10, ], right_df[1:10, ],
    vars = c("age"),
    method = "hungarian"
  )

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with method='jv' works", {
  skip_on_cran()
  result <- match_couples(
    left_df[1:10, ], right_df[1:10, ],
    vars = c("age"),
    method = "jv"
  )

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# detect_blocking helper tests
# ------------------------------------------------------------------------------

test_that("detect_blocking returns FALSE when ignore_blocks is TRUE", {
  skip_on_cran()
  left_blocked <- left_df
  left_blocked$block_id <- rep("A", 20)
  right_blocked <- right_df
  right_blocked$block_id <- rep("A", 20)

  result <- couplr:::detect_blocking(left_blocked, right_blocked, NULL, ignore_blocks = TRUE)

  expect_false(result$use_blocking)
  expect_null(result$block_col)
})

test_that("detect_blocking auto-detects block_id column", {
  skip_on_cran()
  left_blocked <- left_df
  left_blocked$block_id <- rep(c("A", "B"), each = 10)
  right_blocked <- right_df
  right_blocked$block_id <- rep(c("A", "B"), each = 10)

  result <- couplr:::detect_blocking(left_blocked, right_blocked, NULL, ignore_blocks = FALSE)

  expect_true(result$use_blocking)
  expect_equal(result$block_col, "block_id")
})

test_that("detect_blocking returns FALSE when no block column", {
  skip_on_cran()
  result <- couplr:::detect_blocking(left_df, right_df, NULL, ignore_blocks = FALSE)

  expect_false(result$use_blocking)
  expect_null(result$block_col)
})

# ------------------------------------------------------------------------------
# check_full_matching helper tests
# ------------------------------------------------------------------------------

test_that("check_full_matching passes when all matched", {
  skip_on_cran()
  result <- list(
    unmatched = list(left = character(0), right = character(0))
  )

  expect_invisible(couplr:::check_full_matching(result))
})

test_that("check_full_matching errors when unmatched exist", {
  skip_on_cran()
  result <- list(
    unmatched = list(left = c("L1", "L2"), right = c("R1"))
  )

  expect_error(
    couplr:::check_full_matching(result),
    "Full matching required"
  )
})
