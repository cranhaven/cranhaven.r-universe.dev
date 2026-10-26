# ==============================================================================
# Tests for error handling paths across the package
# ==============================================================================

# ------------------------------------------------------------------------------
# assignment() error handling
# ------------------------------------------------------------------------------

test_that("assignment errors on NaN values", {
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)

  expect_error(assignment(cost), "NaN")
})

test_that("assignment errors on empty matrix", {
  cost <- matrix(numeric(0), nrow = 0, ncol = 0)

  expect_error(assignment(cost), "at least one row")
})

test_that("assignment errors on non-matrix input", {
  expect_error(
    assignment("not a matrix"),
    NULL  # Some error expected
  )
})

test_that("assignment handles all-NA matrix", {
  cost <- matrix(NA, 2, 2)

  # Should either error or return infeasible status
  result <- tryCatch(
    assignment(cost),
    error = function(e) "error"
  )

  # Either an error or a result with status != "optimal"
  if (identical(result, "error")) {
    expect_true(TRUE)  # Error is acceptable
 } else {
    expect_true(result$status != "optimal" || is.infinite(result$total_cost))
  }
})

test_that("assignment handles all-Inf matrix", {
  cost <- matrix(Inf, 2, 2)

  # All-Inf means infeasible - should error
  expect_error(assignment(cost), "Infeasible")
})

# ------------------------------------------------------------------------------
# lap_solve() error handling
# ------------------------------------------------------------------------------

test_that("lap_solve errors on non-numeric matrix", {
  cost <- matrix(c("a", "b", "c", "d"), 2, 2)

  expect_error(lap_solve(cost), "numeric")
})

test_that("lap_solve errors on data frame without required columns", {
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(lap_solve(df), NULL)  # Some error expected
})

# ------------------------------------------------------------------------------
# match_couples() error handling
# ------------------------------------------------------------------------------

test_that("match_couples errors on non-data-frame left", {
  right <- data.frame(id = 1:5, x = rnorm(5))

  expect_error(
    match_couples(list(), right, vars = "x"),
    "must be a data frame"
  )
})

test_that("match_couples errors on empty left data frame", {
  left <- data.frame(id = integer(0), x = numeric(0))
  right <- data.frame(id = 1:5, x = rnorm(5))

  expect_error(
    match_couples(left, right, vars = "x"),
    "at least one row"
  )
})

test_that("match_couples errors on empty right data frame", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = integer(0), x = numeric(0))

  expect_error(
    match_couples(left, right, vars = "x"),
    "at least one row"
  )
})

test_that("match_couples errors on missing variable in left", {
  left <- data.frame(id = 1:5, y = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    match_couples(left, right, vars = "x"),
    "missing.*x"
  )
})

test_that("match_couples errors on missing variable in right", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, y = rnorm(5))

  expect_error(
    match_couples(left, right, vars = "x"),
    "missing.*x"
  )
})

test_that("match_couples errors when no valid pairs after constraints", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(100, 200, 300))

  expect_error(
    suppressWarnings(match_couples(left, right, vars = "x", max_distance = 1)),
    "No valid pairs"
  )
})

# ------------------------------------------------------------------------------
# match_couples(method = "greedy") error handling
# ------------------------------------------------------------------------------

test_that("greedy matching errors on non-data-frame input", {
  right <- data.frame(id = 1:5, x = rnorm(5))

  expect_error(
    match_couples("not a df", right, vars = "x", method = "greedy"),
    "must be a data frame"
  )
})

test_that("greedy matching errors on missing vars", {
  left <- data.frame(id = 1:5, y = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    match_couples(left, right, vars = "x", method = "greedy"),
    "missing"
  )
})

# ------------------------------------------------------------------------------
# balance_diagnostics() error handling
# ------------------------------------------------------------------------------

test_that("balance_diagnostics errors on non-matching_result", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))

  expect_error(
    balance_diagnostics("not a result", left, right, vars = "x"),
    "matching_result"
  )
})

test_that("balance_diagnostics errors on missing ID column", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")

  expect_error(
    balance_diagnostics(result, left, right, vars = "x", left_id = "missing"),
    "not found"
  )
})

test_that("balance_diagnostics errors on missing variable", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")

  expect_error(
    balance_diagnostics(result, left, right, vars = "missing_var"),
    "not found"
  )
})

# ------------------------------------------------------------------------------
# join_matched() error handling
# ------------------------------------------------------------------------------

test_that("join_matched errors on non-matching_result", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    join_matched(list(), left, right),
    "matching_result"
  )
})

test_that("join_matched errors on non-data-frame left", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")

  expect_error(
    join_matched(result, as.matrix(left), right),
    "data frames"
  )
})

test_that("join_matched errors on missing left ID column", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")

  expect_error(
    join_matched(result, left, right, left_id = "missing"),
    "not found in left"
  )
})

test_that("join_matched errors on wrong suffix length", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")

  expect_error(
    join_matched(result, left, right, suffix = "_only_one"),
    "length 2"
  )
})

test_that("join_matched errors on missing variable selection", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x")

  expect_error(
    join_matched(result, left, right, left_vars = "missing"),
    "not found in left"
  )
})

# ------------------------------------------------------------------------------
# compute_distances() error handling
# ------------------------------------------------------------------------------

test_that("compute_distances errors on non-data-frames", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  expect_error(
    compute_distances(list(), right, vars = "x"),
    "must be data frames"
  )
})

test_that("compute_distances errors on missing ID column", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  expect_error(
    compute_distances(left, right, vars = "x", left_id = "missing"),
    "not found"
  )
})

test_that("compute_distances errors on missing variable", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, y = 6:10)

  expect_error(
    compute_distances(left, right, vars = c("x", "z")),
    "not found"
  )
})

test_that("compute_distances errors on duplicate IDs", {
  left <- data.frame(id = c(1, 1, 2, 3, 4), x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  expect_error(
    compute_distances(left, right, vars = "x"),
    "Duplicate IDs"
  )
})

# ------------------------------------------------------------------------------
# update_constraints() error handling
# ------------------------------------------------------------------------------

test_that("update_constraints errors on non-distance_object", {
  expect_error(
    update_constraints(list(), max_distance = 1.0),
    "must be a distance_object"
  )
})

# ------------------------------------------------------------------------------
# matchmaker() error handling
# ------------------------------------------------------------------------------

test_that("matchmaker validates input data frames", {
  left <- data.frame(id = 1:5, x = rnorm(5))

  expect_error(
    matchmaker(left, list()),
    NULL  # Some error expected
  )
})

test_that("matchmaker errors on group blocking without block_by", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    matchmaker(left, right, block_type = "group"),
    "must specify block_by"
  )
})

test_that("matchmaker errors on cluster blocking without block_vars", {
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))

  expect_error(
    matchmaker(left, right, block_type = "cluster"),
    "must specify block_vars"
  )
})

# ------------------------------------------------------------------------------
# lap_solve_batch() error handling
# ------------------------------------------------------------------------------

test_that("lap_solve_batch errors on empty list", {
  expect_error(
    lap_solve_batch(list()),
    "at least one problem"
  )
})

test_that("lap_solve_batch errors on invalid type", {
  expect_error(
    lap_solve_batch("not a list"),
    "must be a list"
  )
})

# ------------------------------------------------------------------------------
# lap_solve_kbest() error handling
# ------------------------------------------------------------------------------

test_that("lap_solve_kbest handles k=0 gracefully", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  # k=0 returns empty result (not an error)
  result <- lap_solve_kbest(cost, k = 0)
  expect_equal(nrow(result), 0)
})

test_that("lap_solve_kbest handles k larger than possible solutions", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  # There are only 2! = 2 possible solutions for 2x2
  result <- lap_solve_kbest(cost, k = 10)

  # Should return at most 2 solutions
  expect_true(length(unique(result$solution_id)) <= 2)
})

# ------------------------------------------------------------------------------
# bottleneck_assignment() edge cases
# ------------------------------------------------------------------------------

test_that("bottleneck_assignment handles 1x1 matrix", {
  cost <- matrix(5, 1, 1)

  result <- bottleneck_assignment(cost)

  expect_equal(result$bottleneck, 5)
})

test_that("bottleneck_assignment handles all same values", {
  cost <- matrix(5, 3, 3)

  result <- bottleneck_assignment(cost)

  expect_equal(result$bottleneck, 5)
})

# ------------------------------------------------------------------------------
# sinkhorn() edge cases
# ------------------------------------------------------------------------------

test_that("sinkhorn handles uniform distributions", {
  cost <- matrix(1, 3, 3)

  result <- sinkhorn(cost)

  expect_type(result, "list")
  expect_true(result$converged)
})

test_that("sinkhorn handles large lambda (sharp assignment)", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  # Large lambda = sharper assignment (like small epsilon in other formulations)
  result <- sinkhorn(cost, lambda = 100, max_iter = 1000)

  expect_type(result, "list")
  expect_true(result$converged)
})
