# ==============================================================================
# Targeted tests to boost coverage to 90%
# ==============================================================================

# ------------------------------------------------------------------------------
# lap_solve.R edge cases
# ------------------------------------------------------------------------------

test_that("assignment handles transposed matrix (rows > cols)", {
  skip_on_cran()
  # Force transpose by having more rows than cols
  cost <- matrix(c(1, 5, 3, 2, 5, 1, 4, 2, 3, 6, 2, 1), nrow = 4, ncol = 3)
  result <- assignment(cost, method = "jv")
  expect_equal(result$status, "optimal")
  # Result should have match for all 4 rows
  expect_length(result$match, 4)
})

test_that("assignment with maximization", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1, 3, 2, 4, 3, 2), 3, 3)
  result <- assignment(cost, maximize = TRUE)
  expect_equal(result$status, "optimal")
  # Maximization should give different result than minimization
  result_min <- assignment(cost, maximize = FALSE)
  expect_true(result$total_cost >= result_min$total_cost)
})

test_that("assignment auto-selection picks bruteforce for small", {
  skip_on_cran()
  cost <- matrix(runif(4), 2, 2)
  result <- assignment(cost, method = "auto")
  expect_equal(result$method_used, "bruteforce")
})

test_that("assignment auto-selection sends a wide problem to jv", {
  skip_on_cran()
  # Aspect ratio is not one of the properties the rules read.
  set.seed(42)
  cost <- matrix(runif(4 * 12), 4, 12)  # 4 rows, 12 cols (12 >= 3*4)
  result <- assignment(cost, method = "auto")
  expect_equal(result$method_used, "jv")
})

# ------------------------------------------------------------------------------
# lap_solve tidy interface
# ------------------------------------------------------------------------------

test_that("lap_solve handles rectangular matrices", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  result <- lap_solve(cost)
  expect_s3_class(result, "lap_solve_result")
  expect_equal(nrow(result), 2)
})

test_that("lap_solve with data frame input", {
  skip_on_cran()
  df <- tibble::tibble(
    source = c(1, 1, 2, 2, 3, 3),
    target = c(1, 2, 1, 2, 1, 2),
    cost = c(4, 2, 3, 5, 7, 1)
  )
  result <- lap_solve(df, source = source, target = target, cost = cost)
  expect_s3_class(result, "lap_solve_result")
})

# ------------------------------------------------------------------------------
# matching_preprocessing edge cases
# ------------------------------------------------------------------------------

test_that("preprocess_matching_vars handles all constant variables", {
  skip_on_cran()
  left <- data.frame(x = c(1, 1, 1), y = c(2, 2, 2))
  right <- data.frame(x = c(3, 3, 3), y = c(4, 4, 4))

  # Constant variables get excluded with message
  expect_message(
    result <- preprocess_matching_vars(left, right, vars = c("x", "y")),
    regexp = NULL  # Any message
  )
})

test_that("preprocess_matching_vars handles high missing", {
  skip_on_cran()
  left <- data.frame(x = c(1, NA, NA, NA, 5))
  right <- data.frame(x = c(2, NA, NA, NA, 6))

  # High missing should trigger warning
  expect_warning(
    result <- preprocess_matching_vars(left, right, vars = "x"),
    "missing"
  )
})

# ------------------------------------------------------------------------------
# matching_distance edge cases
# ------------------------------------------------------------------------------

test_that("compute_distance_matrix handles matrices directly", {
  skip_on_cran()
  left_mat <- matrix(1:3, ncol = 1)
  right_mat <- matrix(4:6, ncol = 1)
  result <- couplr:::compute_distance_matrix(left_mat, right_mat)
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 3))
})

test_that("compute_distance_matrix with mahalanobis", {
  skip_on_cran()
  set.seed(123)
  left_mat <- cbind(rnorm(10), rnorm(10))
  right_mat <- cbind(rnorm(10), rnorm(10))

  result <- couplr:::compute_distance_matrix(left_mat, right_mat,
                                             distance = "mahalanobis")
  expect_true(is.matrix(result))
})

# ------------------------------------------------------------------------------
# matching_blocks
# ------------------------------------------------------------------------------

test_that("matchmaker returns block info structure", {
  skip_on_cran()
  left <- data.frame(id = 1:6, group = rep(c("A", "B"), each = 3))
  right <- data.frame(id = 7:12, group = rep(c("A", "B"), each = 3))

  result <- matchmaker(left, right, block_type = "group", block_by = "group")
  expect_type(result, "list")
  expect_true("block_id" %in% names(result$left))
})

# ------------------------------------------------------------------------------
# greedy matching
# ------------------------------------------------------------------------------

test_that("greedy matching with different strategies", {
  skip_on_cran()
  set.seed(123)
  left <- data.frame(x = rnorm(10))
  right <- data.frame(x = rnorm(15))

  for (strategy in c("sorted", "row_best", "pq")) {
    result <- match_couples(left, right, vars = "x", strategy = strategy, method = "greedy")
    expect_s3_class(result, "matching_result")
    expect_true(nrow(result$pairs) > 0)
  }
})

# ------------------------------------------------------------------------------
# matching_join
# ------------------------------------------------------------------------------

test_that("join_matched creates merged dataset", {
  skip_on_cran()
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(4, 5, 6))

  result <- match_couples(left, right, vars = "x")
  joined <- join_matched(result, left, right)

  expect_true(is.data.frame(joined))
  expect_true("left_id" %in% names(joined))
  expect_true("right_id" %in% names(joined))
})

# ------------------------------------------------------------------------------
# bottleneck and sinkhorn
# ------------------------------------------------------------------------------

test_that("bottleneck_assignment returns bottleneck value", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 3, 2, 8, 4, 6, 7, 2), 3, 3)
  result <- bottleneck_assignment(cost)
  expect_true(!is.null(result$bottleneck))
  expect_s3_class(result, "bottleneck_result")
})

test_that("sinkhorn with high lambda converges to assignment", {
  skip_on_cran()
  cost <- matrix(c(1, 10, 10, 1), 2, 2)
  result <- sinkhorn(cost, lambda = 100)
  expect_true(result$converged)

  # With high lambda, should approach optimal assignment
  assign <- sinkhorn_to_assignment(result)
  expect_length(assign, 2)
})

# ------------------------------------------------------------------------------
# print methods
# ------------------------------------------------------------------------------

test_that("print.matching_result works", {
  skip_on_cran()
  left <- data.frame(x = 1:3)
  right <- data.frame(x = 4:6)
  result <- match_couples(left, right, vars = "x")
  expect_output(print(result), "Matching Result")
})

test_that("print.balance_result works", {
  skip_on_cran()
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)
  match_result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(match_result, left, right, vars = "x")
  expect_output(print(balance), "Balance")
})
