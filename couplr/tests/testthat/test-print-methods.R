# ==============================================================================
# Tests for print/summary methods
# ==============================================================================

# ------------------------------------------------------------------------------
# lap_solve_result print tests
# ------------------------------------------------------------------------------

test_that("print.lap_solve_result works", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost)

  expect_output(print(result), "Assignment Result")
})

# ------------------------------------------------------------------------------
# lap_solve_batch_result print tests
# ------------------------------------------------------------------------------

test_that("print.lap_solve_batch_result works with valid result", {
  costs <- list(
    matrix(c(1, 2, 3, 4), 2, 2),
    matrix(c(5, 6, 7, 8), 2, 2)
  )
  result <- lap_solve_batch(costs)

  expect_output(print(result), "Batch Assignment Results")
  expect_output(print(result), "Number of problems solved")
  expect_output(print(result), "Total cost range")
})

test_that("print.lap_solve_batch_result handles missing columns gracefully", {
  # Create a minimal result with stripped columns
  result <- tibble::tibble(x = 1:3)
  class(result) <- c("lap_solve_batch_result", class(result))

  # Should not error, just print what's available
  expect_output(print(result), "Batch Assignment Results")
})

# ------------------------------------------------------------------------------
# lap_solve_kbest_result print tests
# ------------------------------------------------------------------------------

test_that("print.lap_solve_kbest_result works", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve_kbest(cost, k = 2)

  expect_output(print(result), "K-Best")
})

# ------------------------------------------------------------------------------
# matching_result print tests
# ------------------------------------------------------------------------------

test_that("print.matching_result works for optimal matching", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))
  result <- match_couples(left, right, vars = "x")

  expect_output(print(result), "Matching Result")
  expect_output(print(result), "Matched pairs")
})

test_that("print.matching_result works for greedy matching", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.1, 3.1, 4.1, 5.1))
  result <- match_couples(left, right, vars = "x", method = "greedy")

  expect_output(print(result), "Matching Result")
  expect_output(print(result), "greedy")
})

# ------------------------------------------------------------------------------
# matchmaker_result print tests
# ------------------------------------------------------------------------------

test_that("print.matchmaker_result works for group blocking", {
  left <- data.frame(
    id = 1:10,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )
  right <- data.frame(
    id = 11:20,
    region = rep(c("A", "B"), each = 5),
    x = rnorm(10)
  )

  result <- matchmaker(left, right, block_type = "group", block_by = "region")

  expect_output(print(result), "Matchmaker Result")
  expect_output(print(result), "Block type")
  expect_output(print(result), "group")
})

test_that("print.matchmaker_result works for cluster blocking", {
  set.seed(123)
  left <- data.frame(id = 1:20, x = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20))

  result <- matchmaker(
    left, right,
    block_type = "cluster",
    block_vars = "x",
    n_blocks = 3
  )

  expect_output(print(result), "Matchmaker Result")
  expect_output(print(result), "cluster")
})

test_that("print.matchmaker_result shows dropped blocks info", {
  left <- data.frame(
    id = 1:6,
    region = c(rep("A", 5), "B"),
    x = rnorm(6)
  )
  right <- data.frame(
    id = 7:12,
    region = c(rep("A", 5), "B"),
    x = rnorm(6)
  )

  result <- matchmaker(
    left, right,
    block_type = "group",
    block_by = "region",
    min_left = 2
  )

  expect_output(print(result), "Blocks dropped")
})

# ------------------------------------------------------------------------------
# balance_diagnostics print tests
# ------------------------------------------------------------------------------

test_that("print.balance_diagnostics works", {
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

  expect_output(print(balance), "Balance Diagnostics")
  expect_output(print(balance), "Matched pairs")
  expect_output(print(balance), "Overall Balance")
})

test_that("print.balance_diagnostics shows unmatched info", {
  set.seed(456)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 0, 1)
  )
  right <- data.frame(
    id = 11:30,
    x = rnorm(20, 0, 1)
  )

  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")

  expect_output(print(balance), "Unmatched")
})

# ------------------------------------------------------------------------------
# distance_object print tests
# ------------------------------------------------------------------------------

test_that("print.distance_object works", {
  left <- data.frame(id = 1:5, x = 1:5, y = 2:6)
  right <- data.frame(id = 6:10, x = 1:5, y = 2:6)

  dist_obj <- compute_distances(left, right, vars = c("x", "y"))

  expect_output(print(dist_obj), "Distance Object")
  expect_output(print(dist_obj), "Left units")
  expect_output(print(dist_obj), "Variables")
  expect_output(print(dist_obj), "Distance Summary")
})

test_that("summary.distance_object works", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  dist_obj <- compute_distances(left, right, vars = "x")

  expect_output(summary(dist_obj), "Distance Object Summary")
  expect_output(summary(dist_obj), "Quantiles")
  expect_output(summary(dist_obj), "Sparsity")
})

# ------------------------------------------------------------------------------
# preprocessing_result print tests
# ------------------------------------------------------------------------------

test_that("print.preprocessing_result works", {
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

  expect_output(print(preproc), "Preprocessing Result")
})

# ------------------------------------------------------------------------------
# variable_health print tests
# ------------------------------------------------------------------------------

test_that("print.variable_health works", {
  left <- data.frame(
    x = rep(5, 10),
    y = rnorm(10)
  )
  right <- data.frame(
    x = rep(5, 10),
    y = rnorm(10)
  )

  health <- check_variable_health(left, right, c("x", "y"))

  expect_output(print(health), "Variable Health")
})

test_that("print.variable_health shows issues", {
  left <- data.frame(
    const = rep(5, 10),
    all_na = rep(NA_real_, 10)
  )
  right <- data.frame(
    const = rep(5, 10),
    all_na = rep(NA_real_, 10)
  )

  health <- check_variable_health(left, right, c("const", "all_na"))

  expect_output(print(health), "Issues")
})

# ------------------------------------------------------------------------------
# bottleneck_result print tests
# ------------------------------------------------------------------------------

test_that("print for bottleneck_assignment works", {
  cost <- matrix(c(1, 5, 3, 2, 4, 6, 7, 8, 9), 3, 3)
  result <- bottleneck_assignment(cost)

  expect_output(print(result), "Bottleneck")
})

# ------------------------------------------------------------------------------
# sinkhorn_result print tests
# ------------------------------------------------------------------------------

test_that("sinkhorn result is printable", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- sinkhorn(cost)

  expect_no_error(print(result))
})
