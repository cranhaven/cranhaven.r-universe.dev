# ==============================================================================
# Additional coverage tests for edge cases
# ==============================================================================

# ------------------------------------------------------------------------------
# matching_constraints.R additional tests
# ------------------------------------------------------------------------------

test_that("apply_max_distance with edge value at threshold", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  # Value exactly at threshold stays unchanged
  result <- couplr:::apply_max_distance(cost, max_distance = 3)

  expect_equal(result[1, 2], 3)
  expect_true(result[2, 2] > 1e15)  # 4 > 3
})

test_that("apply_max_distance with zero value", {
  cost <- matrix(c(0, 1, 2, 3), 2, 2)

  result <- couplr:::apply_max_distance(cost, max_distance = 0.5)

  expect_equal(result[1, 1], 0)
  expect_true(result[2, 1] > 1e15)
})

test_that("apply_calipers with single caliper matching exactly", {
  left <- data.frame(x = c(1, 2))
  right <- data.frame(x = c(2, 4))
  cost <- matrix(1, 2, 2)
  vars <- "x"
  calipers <- list(x = 1)  # Exactly 1 unit allowed

  result <- couplr:::apply_calipers(cost, left, right, calipers, vars)

  expect_equal(result[1, 1], 1)  # diff = 1, allowed
  expect_true(result[1, 2] > 1e15)  # diff = 3, not allowed
  expect_equal(result[2, 1], 1)  # diff = 0, allowed
  expect_true(result[2, 2] > 1e15)  # diff = 2, not allowed
})

test_that("mark_forbidden_pairs with single pair", {
  cost <- matrix(1, 3, 3)
  forbidden <- matrix(c(1, 1), ncol = 2)

  result <- couplr:::mark_forbidden_pairs(cost, forbidden)

  expect_true(result[1, 1] > 1e15)
  expect_equal(sum(result > 1e15), 1)
})

test_that("has_valid_pairs with mixed values", {
  cost <- matrix(c(1, Inf, 2, couplr:::BIG_COST), 2, 2)

  expect_true(couplr:::has_valid_pairs(cost))
})

test_that("count_valid_pairs with single valid pair", {
  cost <- matrix(couplr:::BIG_COST, 3, 3)
  cost[2, 2] <- 5

  expect_equal(couplr:::count_valid_pairs(cost), 1)
})

# ------------------------------------------------------------------------------
# matching_messages.R additional tests
# ------------------------------------------------------------------------------

test_that("couplr_emoji returns correct types", {
  old <- getOption("couplr.emoji")
  on.exit(options(couplr.emoji = old))

  options(couplr.emoji = FALSE)

  types <- c("error", "warning", "info", "success", "heart",
             "broken", "sparkles", "search", "chart", "warning_sign",
             "stop", "check")

  for (type in types) {
    result <- couplr:::couplr_emoji(type)
    expect_type(result, "character")
  }
})

test_that("warn_extreme_costs without problem vars", {
  expect_warning(
    couplr:::warn_extreme_costs(10, 200, 20, NULL),
    "skewed"
  )
})

test_that("warn_many_forbidden with moderate severity", {
  expect_warning(
    couplr:::warn_many_forbidden(55, 50, 100),
    "forbidden"
  )
})

test_that("check_cost_distribution with no extreme ratios", {
  cost <- matrix(runif(100, min = 1, max = 2), 10, 10)

  result <- couplr:::check_cost_distribution(cost, warn = FALSE)

  expect_true(result$valid)
  expect_true(is.na(result$p95) || result$p99 / result$p95 <= 10)
})

test_that("diagnose_distance_matrix with good quality matrix", {
  cost <- matrix(runif(25), 5, 5)

  result <- diagnose_distance_matrix(cost, warn = FALSE)

  expect_equal(result$quality, "good")
})

# ------------------------------------------------------------------------------
# matching_diagnostics.R additional tests
# ------------------------------------------------------------------------------

test_that("standardized_difference with large difference", {
  x1 <- c(1, 2, 3)
  x2 <- c(100, 101, 102)

  result <- couplr:::standardized_difference(x1, x2, pooled = TRUE)

  expect_true(abs(result) > 1)
})

test_that("calculate_var_balance handles KS test failure gracefully", {
  # Very small samples where KS test might fail
  left_vals <- c(1)
  right_vals <- c(2)

  result <- couplr:::calculate_var_balance(left_vals, right_vals, "x")

  # Should not error
  expect_type(result, "list")
  expect_equal(result$variable, "x")
})

test_that("balance_diagnostics infers vars from result when stored", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- match_couples(left, right, vars = "x")

  # Check if vars is stored in result
  if (!is.null(result$info$vars)) {
    # vars = NULL should infer from result
    balance <- balance_diagnostics(result, left, right, vars = NULL)
    expect_s3_class(balance, "balance_diagnostics")
  } else {
    # vars not stored, explicit vars required
    expect_error(
      balance_diagnostics(result, left, right, vars = NULL),
      "vars must be specified"
    )
  }
})

test_that("balance_table with multiple variables", {
  left <- data.frame(id = 1:10, x = rnorm(10), y = rnorm(10), z = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10), y = rnorm(10), z = rnorm(10))

  result <- match_couples(left, right, vars = c("x", "y", "z"))
  balance <- balance_diagnostics(result, left, right, vars = c("x", "y", "z"))
  tbl <- balance_table(balance)

  expect_equal(nrow(tbl), 3)
})

test_that("summary.balance_diagnostics classifies quality correctly", {
  # Create well-matched data
  set.seed(123)
  left <- data.frame(id = 1:20, x = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20))

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")
  smry <- summary(balance)

  expect_true(smry$quality %in% c("Excellent", "Good", "Acceptable", "Poor"))
})

test_that("plot.balance_diagnostics with custom threshold", {
  skip_if_not_installed("graphics")

  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")

  # Should not error with different thresholds
  expect_invisible(plot(balance, type = "love", threshold = 0.05))
  expect_invisible(plot(balance, type = "love", threshold = 0.5))
})

# ------------------------------------------------------------------------------
# matching_parallel.R additional tests
# ------------------------------------------------------------------------------

test_that("setup_parallel handles invalid plan string", {
  skip_if_not(couplr:::can_parallelize())
  skip_on_cran()

  expect_warning(
    result <- couplr:::setup_parallel(parallel = "invalid_plan_name"),
    "Could not set"
  )
})

test_that("restore_parallel handles non-setup state", {
  state <- list(setup = FALSE, original_plan = NULL)

  # Should not error
  expect_silent(couplr:::restore_parallel(state))
})

test_that("parallel_lapply handles additional arguments", {
  add_val <- function(x, val) x + val

  result <- couplr:::parallel_lapply(1:3, add_val, val = 10, parallel = FALSE)

  expect_equal(result, list(11, 12, 13))
})

test_that("match_blocks_parallel handles blocks with only left data", {
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

  result <- couplr:::match_blocks_parallel(
    blocks = c("A"),  # Only request block A
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

  # Block A has left but no right data
  expect_equal(nrow(result$pairs), 0)
  expect_equal(result$block_summary$n_matched, 0)
})

test_that("greedy_blocks_parallel handles blocks with only right data", {
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
    blocks = c("B"),  # Only request block B
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

  # Block B has right but no left data
  expect_equal(nrow(result$pairs), 0)
})

# ------------------------------------------------------------------------------
# matching_blocks.R additional tests
# ------------------------------------------------------------------------------

test_that("matchmaker auto-determines n_blocks", {
  set.seed(123)
  left <- data.frame(id = 1:50, x = rnorm(50))
  right <- data.frame(id = 51:100, x = rnorm(50))

  result <- matchmaker(
    left, right,
    block_type = "cluster",
    block_vars = "x",
    n_blocks = NULL
  )

  expect_true(result$info$n_blocks_kept >= 1)
})

test_that("filter_blocks handles imbalance threshold correctly", {
  left <- data.frame(
    id = 1:15,
    block_id = c(rep("A", 10), rep("B", 5))
  )
  right <- data.frame(
    id = 16:25,
    block_id = c(rep("A", 5), rep("B", 5))  # A has 5, B has 5
  )

  # A is imbalanced (10 left vs 5 right = 0.5 ratio)
  result <- couplr:::filter_blocks(
    left, right,
    min_left = 1,
    min_right = 1,
    drop_imbalanced = TRUE,
    imbalance_threshold = 0.3
  )

  # A should be dropped due to imbalance
  expect_true("A" %in% result$dropped$blocks)
  expect_equal(result$dropped$reason[result$dropped$blocks == "A"], "imbalanced")
})

test_that("summarize_blocks handles missing block_vars gracefully", {
  left <- data.frame(id = 1:4, block_id = c("A", "A", "B", "B"))
  right <- data.frame(id = 5:8, block_id = c("A", "A", "B", "B"))

  result <- couplr:::summarize_blocks(left, right, block_vars = "nonexistent")

  # Should still work, just without mean columns
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

# ------------------------------------------------------------------------------
# matching_preprocessing.R additional tests
# ------------------------------------------------------------------------------

test_that("check_variable_health summary has all expected columns", {
  left <- data.frame(x = 1:10, y = rnorm(10))
  right <- data.frame(x = 11:20, y = rnorm(10))

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))

  expected_cols <- c("variable", "n_total", "n_na", "prop_na", "n_valid",
                     "mean", "sd", "min", "max", "range", "skewness", "issue")

  for (col in expected_cols) {
    expect_true(col %in% names(result$summary), info = paste("Missing column:", col))
  }
})

test_that("check_variable_health with normal data has no issues", {
  set.seed(123)
  left <- data.frame(x = rnorm(100), y = rnorm(100))
  right <- data.frame(x = rnorm(100), y = rnorm(100))

  result <- couplr:::check_variable_health(left, right, vars = c("x", "y"))

  # Normal data should have no exclusions
  expect_equal(length(result$exclude_vars), 0)
})

# ------------------------------------------------------------------------------
# utils.R additional tests
# ------------------------------------------------------------------------------

test_that("validate_cost_data with single element matrix", {
  cost <- matrix(5, 1, 1)

  result <- couplr:::validate_cost_data(cost)

  expect_equal(dim(result), c(1, 1))
  expect_equal(result[1, 1], 5)
})

test_that("as_assignment_matrix with empty result and no dimensions", {
  result <- tibble::tibble(
    source = integer(0),
    target = integer(0),
    cost = numeric(0)
  )
  class(result) <- c("lap_solve_result", class(result))
  attr(result, "total_cost") <- 0
  attr(result, "method_used") <- "test"

  mat <- as_assignment_matrix(result)

  expect_equal(dim(mat), c(0, 0))
})

test_that("get_total_cost handles batch result with single problem", {
  costs <- list(matrix(c(1, 2, 3, 4), 2, 2))
  result <- lap_solve_batch(costs)

  tc <- get_total_cost(result)

  expect_length(tc, 1)
})

test_that("get_method_used handles kbest result", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve_kbest(cost, k = 2)

  # kbest doesn't have method_used in get_method_used
  expect_error(
    get_method_used(result),
    "not a valid assignment result"
  )
})
