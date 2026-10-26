# ==============================================================================
# Tests for matching layer
# ==============================================================================

test_that("matchmaker creates blocks correctly", {
  left <- data.frame(
    id     = 1:10,
    region = rep(c("A", "B"), each = 5),
    x      = rnorm(10)
  )
  right <- data.frame(
    id     = 11:20,
    region = rep(c("A", "B"), each = 5),
    x      = rnorm(10)
  )

  blocks <- matchmaker(left, right, block_type = "group", block_by = "region")

  expect_s3_class(blocks, "matchmaker_result")
  expect_true("block_id" %in% names(blocks$left))
  expect_true("block_id" %in% names(blocks$right))
  expect_equal(nrow(blocks$block_summary), 2)
  expect_equal(blocks$block_summary$n_left,  c(5, 5))
  expect_equal(blocks$block_summary$n_right, c(5, 5))
})

test_that("matchmaker filters small blocks", {
  left <- data.frame(
    id     = 1:8,
    region = c(rep("A", 5), rep("B", 2), "C"),
    x      = rnorm(8)
  )
  right <- data.frame(
    id     = 11:18,
    region = c(rep("A", 5), rep("B", 2), "C"),
    x      = rnorm(8)
  )

  blocks <- matchmaker(
    left, right,
    block_type = "group",
    block_by   = "region",
    min_left   = 3,
    min_right  = 3
  )

  expect_equal(nrow(blocks$block_summary), 1)
  expect_equal(blocks$block_summary$block_id, "A")
  expect_equal(blocks$info$n_blocks_dropped, 2)
})

test_that("match_couples works with simple input", {
  set.seed(123)
  left  <- data.frame(id = 1:5,  x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x")

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 5)
  expect_true(all(result$pairs$distance < 0.2))
})

test_that("match_couples respects max_distance", {
  left  <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 5, 3.1))

  result <- match_couples(left, right, vars = "x", max_distance = 0.5,
                          check_costs = FALSE)

  expect_equal(nrow(result$pairs), 2)
  expect_true(all(result$pairs$distance <= 0.5))
})

test_that("match_couples respects calipers", {
  left <- data.frame(
    id = 1:3,
    x  = c(1, 2, 3),
    y  = c(10, 20, 30)
  )
  right <- data.frame(
    id = 4:6,
    x  = c(1.1, 2.1, 3.1),
    y  = c(10.5, 25, 30.5)
  )

  result <- match_couples(
    left, right,
    vars        = c("x", "y"),
    calipers    = list(y = 2),
    check_costs = FALSE
  )

  # Should exclude middle pair where y differs by 5
  expect_equal(nrow(result$pairs), 2)
})

test_that("match_couples works with blocking", {
  left <- data.frame(
    id       = 1:6,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1, 2, 3, 10, 20, 30)
  )
  right <- data.frame(
    id       = 7:12,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1.1, 2.1, 3.1, 10.1, 20.1, 30.1)
  )

  result <- match_couples(left, right, vars = "x")

  expect_equal(nrow(result$pairs), 6)
  expect_true("block_id" %in% names(result$pairs))
  expect_equal(sum(result$pairs$block_id == "A"), 3)
  expect_equal(sum(result$pairs$block_id == "B"), 3)
})

test_that("match_couples handles rectangular inputs", {
  left  <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:8, x = c(1.1, 2.1, 3.1, 4, 5))

  result <- match_couples(left, right, vars = "x")

  expect_equal(nrow(result$pairs), 3)
  expect_equal(length(result$unmatched$left),  0)
  expect_equal(length(result$unmatched$right), 2)
})

test_that("validate_matching_inputs catches errors", {
  expect_error(
    validate_matching_inputs(list(), data.frame(x = 1)),
    "left must be a data frame"
  )

  expect_error(
    validate_matching_inputs(data.frame(), data.frame(x = 1)),
    "left must have at least one row"
  )

  expect_error(
    validate_matching_inputs(
      data.frame(x = 1),
      data.frame(y = 1),
      vars = "x"
    ),
    "right.*missing.*x"
  )
})

test_that("distance metrics work correctly", {
  left  <- matrix(c(0, 0), ncol = 2)
  right <- matrix(c(3, 4), ncol = 2)

  # Euclidean: sqrt(3^2 + 4^2) = 5
  d_euclidean <- compute_distance_matrix(left, right, "euclidean")
  expect_equal(d_euclidean[1, 1], 5)

  # Manhattan: |3| + |4| = 7
  d_manhattan <- compute_distance_matrix(left, right, "manhattan")
  expect_equal(d_manhattan[1, 1], 7)

  # Squared Euclidean: 3^2 + 4^2 = 25
  d_sq <- compute_distance_matrix(left, right, "squared_euclidean")
  expect_equal(d_sq[1, 1], 25)
})

test_that("scaling works correctly", {
  left  <- matrix(c(1, 2, 3, 4), ncol = 2)
  right <- matrix(c(5, 6, 7, 8), ncol = 2)

  scaled <- apply_scaling(left, right, "standardize")

  # Check that combined data has mean ~ 0, sd ~ 1
  combined_scaled <- rbind(scaled$left, scaled$right)
  expect_equal(colMeans(combined_scaled), c(0, 0), tolerance = 1e-10)
  expect_equal(apply(combined_scaled, 2, sd), c(1, 1), tolerance = 1e-10)
})

# ==============================================================================
# Tests for match_couples(method = "greedy")
# ==============================================================================

test_that("greedy matching works with simple input", {
  set.seed(123)
  left  <- data.frame(id = 1:5,  x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$method, "greedy")
  expect_equal(nrow(result$pairs), 5)
  expect_true(all(result$pairs$distance < 0.2))
})

test_that("greedy matching respects constraints", {
  left  <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 5, 3.1))

  result <- match_couples(left, right, vars = "x", max_distance = 0.5, method = "greedy")

  expect_equal(nrow(result$pairs), 2)
  expect_true(all(result$pairs$distance <= 0.5))
})

test_that("greedy matching works with blocking", {
  left <- data.frame(
    id       = 1:6,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1, 2, 3, 10, 20, 30)
  )
  right <- data.frame(
    id       = 7:12,
    block_id = rep(c("A", "B"), each = 3),
    x        = c(1.1, 2.1, 3.1, 10.1, 20.1, 30.1)
  )

  result <- match_couples(left, right, vars = "x", method = "greedy")

  expect_equal(nrow(result$pairs), 6)
  expect_true("block_id" %in% names(result$pairs))
  expect_equal(result$info$n_blocks, 2)
})

test_that("greedy strategies produce valid matchings", {
  set.seed(456)
  left  <- data.frame(id = 1:20,  x = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20))

  result_row    <- match_couples(left, right, vars = "x", strategy = "row_best", method = "greedy")
  result_sorted <- match_couples(left, right, vars = "x", strategy = "sorted", method = "greedy")
  result_pq     <- match_couples(left, right, vars = "x", strategy = "pq", method = "greedy")

  # All should produce valid matchings
  expect_equal(nrow(result_row$pairs),    20)
  expect_equal(nrow(result_sorted$pairs), 20)
  expect_equal(nrow(result_pq$pairs),     20)

  # No duplicate assignments
  expect_equal(length(unique(result_row$pairs$left_id)),  20)
  expect_equal(length(unique(result_row$pairs$right_id)), 20)
})

test_that("greedy is faster than optimal (smoke test)", {
  skip_if_not(requireNamespace("microbenchmark", quietly = TRUE))

  set.seed(789)
  left  <- data.frame(id = 1:50,  x = rnorm(50), y = rnorm(50))
  right <- data.frame(id = 51:100, x = rnorm(50), y = rnorm(50))

  # Just test that both complete successfully
  expect_no_error({
    result_opt    <- match_couples(left, right, vars = c("x", "y"))
    result_greedy <- match_couples(left, right, vars = c("x", "y"), method = "greedy")
  })

  # Greedy should produce valid but possibly suboptimal matching
  expect_gte(result_greedy$info$total_distance, result_opt$info$total_distance)
})

# ==============================================================================
# Tests for preprocessing functions
# ==============================================================================

test_that("check_variable_health detects constant variables", {
  left <- data.frame(
    x = rep(5, 10),
    y = rnorm(10)
  )
  right <- data.frame(
    x = rep(5, 10),
    y = rnorm(10)
  )

  health <- check_variable_health(left, right, c("x", "y"))

  expect_s3_class(health, "variable_health")
  expect_true("x" %in% health$exclude_vars)
  expect_false("y" %in% health$exclude_vars)
  expect_true(any(sapply(health$issues, function(i) i$type == "constant")))
})

test_that("check_variable_health detects all-NA variables", {
  left <- data.frame(
    x = rep(NA_real_, 10),
    y = rnorm(10)
  )
  right <- data.frame(
    x = rep(NA_real_, 10),
    y = rnorm(10)
  )

  health <- check_variable_health(left, right, c("x", "y"))

  expect_true("x" %in% health$exclude_vars)
  expect_true(any(sapply(health$issues, function(i) i$type == "all_na")))
})

test_that("check_variable_health detects high missingness", {
  left <- data.frame(
    x = c(rep(NA_real_, 7), rnorm(3))
  )
  right <- data.frame(
    x = c(rep(NA_real_, 7), rnorm(3))
  )

  health <- check_variable_health(left, right, "x")

  expect_true(any(sapply(health$issues, function(i) i$type == "high_missingness")))
})

test_that("check_variable_health detects skewness", {
  set.seed(42)
  # Create highly skewed data
  skewed_data <- c(rnorm(45, mean = 0, sd = 1), rnorm(5, mean = 20, sd = 1))

  left <- data.frame(x = skewed_data[1:25])
  right <- data.frame(x = skewed_data[26:50])

  health <- check_variable_health(left, right, "x")

  # Should detect skewness
  expect_true(any(sapply(health$issues, function(i) i$type == "skewed")))
})

test_that("suggest_scaling returns appropriate methods", {
  # Similar scales - should suggest standardize or none
  left <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 20, 3))
  right <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 20, 3))

  method <- suggest_scaling(left, right, c("x", "y"))
  expect_true(method %in% c("standardize", "robust", "none"))

  # Very different scales - should suggest standardize
  left2 <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 1000, 200))
  right2 <- data.frame(x = rnorm(50, 10, 2), y = rnorm(50, 1000, 200))

  method2 <- suggest_scaling(left2, right2, c("x", "y"))
  expect_equal(method2, "standardize")
})

test_that("preprocess_matching_vars excludes problematic variables", {
  left <- data.frame(
    const = rep(5, 10),
    good = rnorm(10),
    all_na = rep(NA_real_, 10)
  )
  right <- data.frame(
    const = rep(5, 10),
    good = rnorm(10),
    all_na = rep(NA_real_, 10)
  )

  preproc <- preprocess_matching_vars(
    left, right,
    vars = c("const", "good", "all_na"),
    auto_scale = TRUE,
    remove_problematic = TRUE,
    verbose = FALSE
  )

  expect_s3_class(preproc, "preprocessing_result")
  expect_equal(preproc$vars, "good")
  expect_setequal(preproc$excluded_vars, c("const", "all_na"))
})

test_that("preprocess_matching_vars suggests scaling method", {
  left <- data.frame(x = rnorm(50, 100, 10), y = rnorm(50, 0.5, 0.1))
  right <- data.frame(x = rnorm(50, 100, 10), y = rnorm(50, 0.5, 0.1))

  preproc <- preprocess_matching_vars(
    left, right,
    vars = c("x", "y"),
    auto_scale = TRUE,
    scale_method = "auto",
    verbose = FALSE
  )

  expect_true(preproc$scaling_method %in% c("standardize", "robust", "range"))
})

test_that("match_couples with auto_scale works correctly", {
  set.seed(123)
  left <- data.frame(
    id = 1:10,
    const = rep(1, 10),  # Should be excluded
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )
  right <- data.frame(
    id = 11:20,
    const = rep(1, 10),  # Should be excluded
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )

  # Suppress warnings about excluded variables
  result <- suppressWarnings({
    match_couples(
      left, right,
      vars = c("const", "x", "y"),
      auto_scale = TRUE
    )
  })

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 10)
})

test_that("greedy matching with auto_scale works correctly", {
  set.seed(456)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )
  right <- data.frame(
    id = 11:20,
    x = rnorm(10, 100, 10),
    y = rnorm(10, 0.5, 0.1)
  )

  result <- match_couples(
    left, right,
    vars = c("x", "y"),
    auto_scale = TRUE
  , method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_equal(result$info$method, "greedy")
})

# ==============================================================================
# Tests for balance diagnostics
# ==============================================================================

test_that("standardized_difference calculates correctly", {
  x1 <- c(10, 12, 14, 16, 18)
  x2 <- c(11, 13, 15, 17, 19)

  std_diff <- standardized_difference(x1, x2)

  # Should be close to -1 / sqrt(var) for shift of 1
  expect_true(is.numeric(std_diff))
  expect_false(is.na(std_diff))
  expect_true(abs(std_diff) < 1)  # Should be small for similar distributions
})

test_that("standardized_difference handles edge cases", {
  # Empty vectors
  expect_true(is.na(standardized_difference(numeric(0), c(1, 2, 3))))

  # Constant values (SD = 0)
  expect_equal(standardized_difference(c(5, 5, 5), c(5, 5, 5)), 0)

  # With NAs
  x1 <- c(1, 2, 3, NA, 5)
  x2 <- c(2, 3, 4, 5, NA)
  std_diff <- standardized_difference(x1, x2)
  expect_false(is.na(std_diff))
})

test_that("balance_diagnostics works with simple matching", {
  set.seed(123)
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 50, 10),
    income = rnorm(20, 50000, 10000)
  )
  right <- data.frame(
    id = 21:50,
    age = rnorm(30, 50, 10),
    income = rnorm(30, 50000, 10000)
  )

  result <- match_couples(left, right, vars = c("age", "income"))
  balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))

  expect_s3_class(balance, "balance_diagnostics")
  expect_equal(nrow(balance$var_stats), 2)
  expect_true("age" %in% balance$var_stats$variable)
  expect_true("income" %in% balance$var_stats$variable)
  expect_equal(balance$n_matched, 20)
  expect_true(is.numeric(balance$overall$mean_abs_std_diff))
})

test_that("balance_diagnostics computes correct statistics", {
  set.seed(456)
  # Create data with known properties
  left <- data.frame(
    id = 1:30,
    x = rep(10, 30)  # Constant value
  )
  right <- data.frame(
    id = 31:60,
    x = rep(10, 30)  # Same constant value
  )

  result <- suppressWarnings(match_couples(left, right, vars = "x"))
  balance <- balance_diagnostics(result, left, right, vars = "x")

  # With identical values, standardized difference should be 0
  expect_equal(balance$var_stats$std_diff[1], 0)
  expect_equal(balance$var_stats$mean_diff[1], 0)
})

test_that("balance_diagnostics detects imbalance", {
  set.seed(789)
  # Create data with deliberate imbalance
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 30, 5)  # Younger group
  )
  right <- data.frame(
    id = 21:50,
    age = rnorm(30, 50, 5)  # Older group
  )

  result <- match_couples(left, right, vars = "age")
  balance <- balance_diagnostics(result, left, right, vars = "age")

  # Should detect large standardized difference
  expect_true(abs(balance$var_stats$std_diff[1]) > 0.1)
  expect_true(balance$overall$mean_abs_std_diff > 0.1)
})

test_that("balance_diagnostics works with blocking", {
  set.seed(321)
  left <- data.frame(
    id = 1:40,
    group = rep(c("A", "B"), each = 20),
    age = rnorm(40, 50, 10)
  )
  right <- data.frame(
    id = 41:100,
    group = rep(c("A", "B"), each = 30),
    age = rnorm(60, 50, 10)
  )

  blocks <- matchmaker(left, right, block_type = "group", block_by = "group")
  result <- match_couples(blocks$left, blocks$right, vars = "age")

  balance <- balance_diagnostics(result, blocks$left, blocks$right, vars = "age")

  expect_true(balance$has_blocks)
  expect_false(is.null(balance$block_stats))
  expect_equal(nrow(balance$block_stats), 2)  # Two blocks
  expect_true("quality" %in% names(balance$block_stats))
})

test_that("balance_diagnostics handles unmatched units", {
  set.seed(654)
  # Create unbalanced data (more right than left)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 0, 1)
  )
  right <- data.frame(
    id = 11:40,
    x = rnorm(30, 0, 1)
  )

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")

  expect_equal(balance$n_matched, 10)
  expect_equal(balance$n_unmatched_left, 0)
  expect_equal(balance$n_unmatched_right, 20)
})

test_that("balance_table formats output correctly", {
  set.seed(987)
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, 50, 10),
    income = rnorm(20, 50000, 10000)
  )
  right <- data.frame(
    id = 21:50,
    age = rnorm(30, 50, 10),
    income = rnorm(30, 50000, 10000)
  )

  result <- match_couples(left, right, vars = c("age", "income"))
  balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))

  tbl <- balance_table(balance)

  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2)
  expect_true("Variable" %in% names(tbl))
  expect_true("Std Diff" %in% names(tbl))
  expect_true("Mean Diff" %in% names(tbl))
})

test_that("balance_diagnostics print method works", {
  set.seed(111)
  left <- data.frame(
    id = 1:15,
    x = rnorm(15, 0, 1)
  )
  right <- data.frame(
    id = 16:40,
    x = rnorm(25, 0, 1)
  )

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")

  # Should print without error
  expect_output(print(balance), "Balance Diagnostics")
  expect_output(print(balance), "Matched pairs")
  expect_output(print(balance), "Overall Balance")
})

test_that("balance_diagnostics validates inputs", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")

  # Wrong result type
  expect_error(
    balance_diagnostics("not a result", left, right, vars = "x"),
    "matching_result"
  )

  # Missing ID column
  expect_error(
    balance_diagnostics(result, left, right, vars = "x", left_id = "missing"),
    "not found"
  )

  # Missing variable
  expect_error(
    balance_diagnostics(result, left, right, vars = "missing_var"),
    "not found"
  )
})

# ==============================================================================
# Tests for join_matched()
# ==============================================================================

test_that("join_matched works with basic matching", {
  left <- data.frame(
    id = 1:5,
    treatment = 1,
    age = c(25, 30, 35, 40, 45),
    income = c(45000, 52000, 48000, 61000, 55000)
  )

  right <- data.frame(
    id = 6:10,
    treatment = 0,
    age = c(24, 29, 36, 41, 44),
    income = c(46000, 51500, 47500, 60000, 54000)
  )

  result <- match_couples(left, right, vars = c("age", "income"))
  joined <- join_matched(result, left, right)

  # Check structure
  expect_s3_class(joined, "tbl_df")
  expect_equal(nrow(joined), nrow(result$pairs))

  # Check required columns
  expect_true("pair_id" %in% names(joined))
  expect_true("left_id" %in% names(joined))
  expect_true("right_id" %in% names(joined))
  expect_true("distance" %in% names(joined))

  # Check suffixes applied
  expect_true("treatment_left" %in% names(joined))
  expect_true("treatment_right" %in% names(joined))
  expect_true("age_left" %in% names(joined))
  expect_true("age_right" %in% names(joined))
  expect_true("income_left" %in% names(joined))
  expect_true("income_right" %in% names(joined))

  # Check values (IDs are type-converted to match original data)
  expect_type(joined$left_id, typeof(left$id))
  expect_type(joined$right_id, typeof(right$id))
  expect_equal(nrow(joined), nrow(result$pairs))
  expect_equal(joined$distance, result$pairs$distance)
})

test_that("join_matched handles custom suffixes", {
  left <- data.frame(
    id = 1:3,
    treatment = 1,
    age = c(25, 30, 35)
  )

  right <- data.frame(
    id = 4:6,
    treatment = 0,
    age = c(24, 29, 36)
  )

  result <- match_couples(left, right, vars = "age")
  joined <- join_matched(
    result, left, right,
    suffix = c("_treated", "_control")
  )

  expect_true("treatment_treated" %in% names(joined))
  expect_true("treatment_control" %in% names(joined))
  expect_true("age_treated" %in% names(joined))
  expect_true("age_control" %in% names(joined))
})

test_that("join_matched handles variable selection", {
  left <- data.frame(
    id = 1:3,
    treatment = 1,
    age = c(25, 30, 35),
    income = c(45000, 52000, 48000),
    extra = c("a", "b", "c")
  )

  right <- data.frame(
    id = 4:6,
    treatment = 0,
    age = c(24, 29, 36),
    income = c(46000, 51500, 47500),
    extra = c("x", "y", "z")
  )

  result <- match_couples(left, right, vars = c("age", "income"))

  # Select specific variables
  joined <- join_matched(
    result, left, right,
    left_vars = c("treatment", "age"),
    right_vars = "age"
  )

  expect_true("treatment_left" %in% names(joined))
  expect_true("age_left" %in% names(joined))
  expect_true("age_right" %in% names(joined))
  expect_false("income_left" %in% names(joined))
  expect_false("income_right" %in% names(joined))
  expect_false("extra_left" %in% names(joined))
  expect_false("extra_right" %in% names(joined))
})

test_that("join_matched works with blocking", {
  left <- data.frame(
    id = 1:10,
    region = rep(c("A", "B"), each = 5),
    age = c(25, 30, 35, 40, 45, 26, 31, 36, 41, 46)
  )

  right <- data.frame(
    id = 11:20,
    region = rep(c("A", "B"), each = 5),
    age = c(24, 29, 36, 41, 44, 25, 30, 37, 42, 45)
  )

  blocks <- matchmaker(left, right, block_type = "group", block_by = "region")
  result <- match_couples(blocks$left, blocks$right, vars = "age")
  joined <- join_matched(result, blocks$left, blocks$right)

  expect_true("block_id" %in% names(joined))
  expect_equal(length(unique(joined$block_id)), 2)
})

test_that("join_matched handles include options", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))

  result <- match_couples(left, right, vars = "x")

  # Without distance
  joined1 <- join_matched(result, left, right, include_distance = FALSE)
  expect_false("distance" %in% names(joined1))

  # Without pair_id
  joined2 <- join_matched(result, left, right, include_pair_id = FALSE)
  expect_false("pair_id" %in% names(joined2))

  # Without both
  joined3 <- join_matched(
    result, left, right,
    include_distance = FALSE,
    include_pair_id = FALSE
  )
  expect_false("distance" %in% names(joined3))
  expect_false("pair_id" %in% names(joined3))
})

test_that("join_matched handles custom ID columns", {
  # Create data with custom ID columns
  left <- data.frame(
    patient_id = paste0("P", 1:3),
    age = c(25, 30, 35)
  )

  right <- data.frame(
    control_id = paste0("C", 4:6),
    age = c(24, 29, 36)
  )

  # For this test, we'll use standard "id" column for matching
  # but demonstrate custom ID specification in join_matched
  left$id <- 1:3
  right$id <- 4:6

  result <- match_couples(
    left, right,
    vars = "age"
  )

  # join_matched can still select custom ID columns from the original data
  joined <- join_matched(
    result, left, right,
    left_vars = c("patient_id", "age"),
    right_vars = c("control_id", "age")
  )

  # Verify structure
  expect_true("left_id" %in% names(joined))
  expect_true("right_id" %in% names(joined))
  expect_true("patient_id_left" %in% names(joined))
  expect_true("control_id_right" %in% names(joined))

  # Verify patient_id and control_id were joined correctly
  expect_equal(joined$patient_id_left, paste0("P", joined$left_id))
  expect_equal(joined$control_id_right, paste0("C", joined$right_id))
})

test_that("join_matched validates inputs", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))
  result <- match_couples(left, right, vars = "x")

  # Not a matching_result
  expect_error(
    join_matched(list(), left, right),
    "matching_result object"
  )

  # Not data frames
  expect_error(
    join_matched(result, as.matrix(left), right),
    "data frames"
  )

  # Missing ID column
  expect_error(
    join_matched(result, left, right, left_id = "missing"),
    "not found in left"
  )

  expect_error(
    join_matched(result, left, right, right_id = "missing"),
    "not found in right"
  )

  # Wrong suffix length
  expect_error(
    join_matched(result, left, right, suffix = "_only_one"),
    "length 2"
  )

  # Missing variables
  expect_error(
    join_matched(result, left, right, left_vars = "missing"),
    "not found in left"
  )

  expect_error(
    join_matched(result, left, right, right_vars = "missing"),
    "not found in right"
  )
})

test_that("join_matched handles empty matches", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(100, 200, 300))

  # With strict caliper, should throw error (no valid pairs)
  expect_error(
    suppressWarnings(match_couples(left, right, vars = "x", max_distance = 1)),
    "No valid pairs"
  )
})

test_that("join_matched works with greedy matching", {
  left <- data.frame(
    id = 1:5,
    treatment = 1,
    age = c(25, 30, 35, 40, 45)
  )

  right <- data.frame(
    id = 6:10,
    treatment = 0,
    age = c(24, 29, 36, 41, 44)
  )

  result <- match_couples(left, right, vars = "age", strategy = "sorted", method = "greedy")
  joined <- join_matched(result, left, right)

  expect_s3_class(joined, "tbl_df")
  expect_equal(nrow(joined), nrow(result$pairs))
  expect_true(all(c("treatment_left", "treatment_right") %in% names(joined)))
})

test_that("augment.matching_result works", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))
  result <- match_couples(left, right, vars = "x")

  # Call augment with explicit namespace
  augmented <- couplr::augment(result, left, right)

  # Should be identical to join_matched with defaults
  joined <- join_matched(result, left, right)
  expect_equal(augmented, joined)

  # Can pass additional arguments
  augmented2 <- couplr::augment(
    result, left, right,
    suffix = c("_a", "_b"),
    include_distance = FALSE
  )
  expect_true("x_a" %in% names(augmented2))
  expect_true("x_b" %in% names(augmented2))
  expect_false("distance" %in% names(augmented2))
})

test_that("join_matched column order is sensible", {
  left <- data.frame(
    id = 1:3,
    treatment = 1,
    age = c(25, 30, 35)
  )

  right <- data.frame(
    id = 4:6,
    treatment = 0,
    age = c(24, 29, 36)
  )

  result <- match_couples(left, right, vars = "age")
  joined <- join_matched(result, left, right)

  # Check order: pair_id, left_id, right_id, distance, then variables
  col_names <- names(joined)
  expect_equal(col_names[1], "pair_id")
  expect_equal(col_names[2], "left_id")
  expect_equal(col_names[3], "right_id")
  expect_equal(col_names[4], "distance")
})


# ==============================================================================
# Distance Caching Tests (Step 4)
# ==============================================================================

test_that("compute_distances creates valid distance object", {
  left <- data.frame(
    id = 1:5,
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 6, 8, 10)
  )
  right <- data.frame(
    id = 6:10,
    x = c(1.1, 2.1, 3.1, 4.1, 5.1),
    y = c(2.2, 4.2, 6.2, 8.2, 10.2)
  )

  dist_obj <- compute_distances(left, right, vars = c("x", "y"))

  # Check class
  expect_s3_class(dist_obj, "distance_object")
  expect_true(is_distance_object(dist_obj))

  # Check structure
  expect_true("cost_matrix" %in% names(dist_obj))
  expect_true("left_ids" %in% names(dist_obj))
  expect_true("right_ids" %in% names(dist_obj))
  expect_true("metadata" %in% names(dist_obj))
  expect_true("original_left" %in% names(dist_obj))
  expect_true("original_right" %in% names(dist_obj))

  # Check dimensions
  expect_equal(nrow(dist_obj$cost_matrix), 5)
  expect_equal(ncol(dist_obj$cost_matrix), 5)
  expect_equal(length(dist_obj$left_ids), 5)
  expect_equal(length(dist_obj$right_ids), 5)

  # Check metadata
  expect_equal(dist_obj$metadata$vars, c("x", "y"))
  expect_equal(dist_obj$metadata$distance, "euclidean")
  expect_equal(dist_obj$metadata$n_left, 5)
  expect_equal(dist_obj$metadata$n_right, 5)

  # Check original datasets stored
  expect_equal(nrow(dist_obj$original_left), 5)
  expect_equal(nrow(dist_obj$original_right), 5)
})

test_that("compute_distances with auto_scale works", {
  set.seed(123)
  left <- data.frame(
    id = 1:20,
    age = rnorm(20, mean = 50, sd = 10),
    income = rnorm(20, mean = 50000, sd = 15000)
  )
  right <- data.frame(
    id = 21:50,
    age = rnorm(30, mean = 52, sd = 10),
    income = rnorm(30, mean = 52000, sd = 15000)
  )

  # Should work with auto_scale
  dist_obj <- compute_distances(
    left, right,
    vars = c("age", "income"),
    auto_scale = TRUE
  )

  # Should have applied auto_scale
  expect_true(dist_obj$metadata$auto_scale)
  expect_s3_class(dist_obj, "distance_object")
})

test_that("compute_distances with scaling works", {
  left <- data.frame(
    id = 1:5,
    x = c(1, 2, 3, 4, 5),
    y = c(10, 20, 30, 40, 50)
  )
  right <- data.frame(
    id = 6:10,
    x = c(1.5, 2.5, 3.5, 4.5, 5.5),
    y = c(15, 25, 35, 45, 55)
  )

  dist_standardize <- compute_distances(left, right, vars = c("x", "y"), scale = "standardize")
  dist_range <- compute_distances(left, right, vars = c("x", "y"), scale = "range")
  dist_none <- compute_distances(left, right, vars = c("x", "y"), scale = FALSE)

  # All should create valid distance objects
  expect_s3_class(dist_standardize, "distance_object")
  expect_s3_class(dist_range, "distance_object")
  expect_s3_class(dist_none, "distance_object")

  # Distances should differ based on scaling
  expect_false(identical(dist_standardize$cost_matrix, dist_range$cost_matrix))
  expect_false(identical(dist_standardize$cost_matrix, dist_none$cost_matrix))

  # Metadata should reflect scaling choice
  expect_equal(dist_standardize$metadata$scale, "standardize")
  expect_equal(dist_range$metadata$scale, "range")
  expect_equal(dist_none$metadata$scale, FALSE)
})

test_that("compute_distances validates inputs", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  # Not data frames
  expect_error(
    compute_distances(list(), right, vars = "x"),
    "must be data frames"
  )

  # Missing ID columns
  expect_error(
    compute_distances(left, right, vars = "x", left_id = "missing"),
    "not found in left dataset"
  )

  # Missing variables
  expect_error(
    compute_distances(left, right, vars = c("x", "z")),
    "not found in both datasets"
  )

  # Duplicate IDs
  left_dup <- data.frame(id = c(1, 1, 2, 3, 4), x = 1:5)
  expect_error(
    compute_distances(left_dup, right, vars = "x"),
    "Duplicate IDs"
  )
})

test_that("match_couples works with distance object", {
  left <- data.frame(
    id = 1:5,
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 6, 8, 10)
  )
  right <- data.frame(
    id = 6:10,
    x = c(1.1, 2.1, 3.1, 4.1, 5.1),
    y = c(2.2, 4.2, 6.2, 8.2, 10.2)
  )

  # Create distance object
  dist_obj <- compute_distances(left, right, vars = c("x", "y"), scale = "standardize")

  # Match using distance object
  result <- match_couples(dist_obj)

  # Should return valid matching_result
  expect_s3_class(result, "matching_result")
  expect_true("pairs" %in% names(result))
  expect_true("info" %in% names(result))

  # Should have matched all 5 pairs
  expect_equal(nrow(result$pairs), 5)

  # Pairs should have expected columns
  expect_true("left_id" %in% names(result$pairs))
  expect_true("right_id" %in% names(result$pairs))
  expect_true("distance" %in% names(result$pairs))
})

test_that("greedy matching works with distance object", {
  left <- data.frame(
    id = 1:5,
    x = c(1, 2, 3, 4, 5)
  )
  right <- data.frame(
    id = 6:10,
    x = c(1.5, 2.5, 3.5, 4.5, 5.5)
  )

  # Create distance object
  dist_obj <- compute_distances(left, right, vars = "x")

  # Greedy matching using distance object
  result <- match_couples(dist_obj, strategy = "sorted", method = "greedy")

  # Should return valid matching_result
  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 5)
})

test_that("distance object reusable across multiple matches", {
  left <- data.frame(
    id = 1:10,
    x = rnorm(10),
    y = rnorm(10)
  )
  right <- data.frame(
    id = 11:30,
    x = rnorm(20),
    y = rnorm(20)
  )

  # Compute distances once
  dist_obj <- compute_distances(left, right, vars = c("x", "y"), scale = "standardize")

  # Use for multiple matching strategies
  result1 <- match_couples(dist_obj, max_distance = 0.5, check_costs = FALSE)
  result2 <- match_couples(dist_obj, max_distance = 1.0, check_costs = FALSE)
  result3 <- match_couples(dist_obj, max_distance = 2.0)
  result4 <- match_couples(dist_obj, strategy = "sorted", method = "greedy")

  # All should be valid
  expect_s3_class(result1, "matching_result")
  expect_s3_class(result2, "matching_result")
  expect_s3_class(result3, "matching_result")
  expect_s3_class(result4, "matching_result")

  # Different max_distance should give different results
  expect_true(nrow(result1$pairs) <= nrow(result2$pairs))
  expect_true(nrow(result2$pairs) <= nrow(result3$pairs))
})

test_that("update_constraints creates new distance object", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.5, 2.5, 3.5, 4.5, 5.5))

  dist_obj <- compute_distances(left, right, vars = "x")

  # Update constraints
  constrained <- update_constraints(dist_obj, max_distance = 1.0)

  # Should be a new object
  expect_s3_class(constrained, "distance_object")

  # Should have constraint metadata
  expect_true("constraints_applied" %in% names(constrained$metadata))
  expect_equal(constrained$metadata$constraints_applied$max_distance, 1.0)

  # Original should be unchanged
  expect_false("constraints_applied" %in% names(dist_obj$metadata))

  # Constrained should have more Inf values
  n_inf_original <- sum(is.infinite(dist_obj$cost_matrix))
  n_inf_constrained <- sum(is.infinite(constrained$cost_matrix))
  expect_true(n_inf_constrained >= n_inf_original)
})

test_that("update_constraints validates input", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  # Not a distance object
  expect_error(
    update_constraints(list(), max_distance = 1.0),
    "must be a distance_object"
  )
})

test_that("distance object with blocking stores block info", {
  left <- data.frame(
    id = 1:10,
    block = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    block = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  expect_message(
    dist_obj <- compute_distances(left, right, vars = "x", block_id = "block"),
    "Block information stored"
  )

  expect_equal(dist_obj$block_id, "block")
})

test_that("print.distance_object works", {
  left <- data.frame(id = 1:5, x = 1:5, y = 2:6)
  right <- data.frame(id = 6:10, x = 1:5, y = 2:6)

  dist_obj <- compute_distances(left, right, vars = c("x", "y"), scale = "standardize")

  # Should not error
  expect_output(print(dist_obj), "Distance Object")
  expect_output(print(dist_obj), "Left units")
  expect_output(print(dist_obj), "Variables")
  expect_output(print(dist_obj), "Distance Summary")
})

test_that("summary.distance_object works", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  dist_obj <- compute_distances(left, right, vars = "x")

  # Should not error
  expect_output(summary(dist_obj), "Distance Object Summary")
  expect_output(summary(dist_obj), "Quantiles")
  expect_output(summary(dist_obj), "Sparsity")
})

test_that("distance object works with join_matched", {
  left <- data.frame(
    id = 1:5,
    treatment = 1,
    x = 1:5,
    y = 2:6
  )
  right <- data.frame(
    id = 6:10,
    treatment = 0,
    x = 1:5,
    y = 2:6
  )

  # Create distance object and match
  dist_obj <- compute_distances(left, right, vars = c("x", "y"))
  result <- suppressWarnings(match_couples(dist_obj))

  # Join should work seamlessly
  joined <- join_matched(result, left, right)

  expect_s3_class(joined, "tbl_df")
  expect_equal(nrow(joined), nrow(result$pairs))
  expect_true("treatment_left" %in% names(joined))
  expect_true("treatment_right" %in% names(joined))
})

test_that("distance object works with balance_diagnostics", {
  set.seed(456)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, mean = 5, sd = 2),
    y = rnorm(10, mean = 10, sd = 3)
  )
  right <- data.frame(
    id = 11:30,
    x = rnorm(20, mean = 5.5, sd = 2),
    y = rnorm(20, mean = 10.5, sd = 3)
  )

  # Create distance object and match
  dist_obj <- compute_distances(left, right, vars = c("x", "y"), scale = "standardize")
  result <- match_couples(dist_obj)

  # Balance diagnostics should work
  balance <- balance_diagnostics(result, left, right, vars = c("x", "y"))

  expect_s3_class(balance, "balance_diagnostics")
  expect_true("var_stats" %in% names(balance))
  expect_true("overall" %in% names(balance))
})

test_that("distance object backwards compatible", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  # Old way (still works)
  result_old <- match_couples(left, right, vars = "x", distance = "euclidean")

  # New way
  dist_obj <- compute_distances(left, right, vars = "x", distance = "euclidean")
  result_new <- match_couples(dist_obj)

  # Should produce identical results
  expect_equal(nrow(result_old$pairs), nrow(result_new$pairs))
  expect_equal(result_old$info$n_matched, result_new$info$n_matched)

  # Distances should be the same
  expect_equal(
    sort(result_old$pairs$distance),
    sort(result_new$pairs$distance)
  )
})

test_that("is_distance_object works correctly", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  dist_obj <- compute_distances(left, right, vars = "x")

  expect_true(is_distance_object(dist_obj))
  expect_false(is_distance_object(list()))
  expect_false(is_distance_object(data.frame()))
  expect_false(is_distance_object(NULL))
})

test_that("distance object with custom ID columns", {
  left <- data.frame(
    patient_id = 101:105,
    x = 1:5
  )
  right <- data.frame(
    control_id = 201:205,
    x = 1:5
  )

  dist_obj <- compute_distances(
    left, right,
    vars = "x",
    left_id = "patient_id",
    right_id = "control_id"
  )

  expect_equal(dist_obj$metadata$left_id, "patient_id")
  expect_equal(dist_obj$metadata$right_id, "control_id")
  expect_equal(dist_obj$left_ids, as.character(101:105))
  expect_equal(dist_obj$right_ids, as.character(201:205))

  # Should work with match_couples
  result <- suppressWarnings(match_couples(dist_obj))
  expect_s3_class(result, "matching_result")
})
