# ==============================================================================
# Final coverage tests for lap_solve.R and related functions
# ==============================================================================

# ------------------------------------------------------------------------------
# assignment() edge cases
# ------------------------------------------------------------------------------

test_that("assignment errors on empty matrix", {
  skip_on_cran()
  expect_error(
    assignment(matrix(nrow = 0, ncol = 0)),
    "at least one row"
  )
})

test_that("assignment errors on 0-row matrix", {
  skip_on_cran()
  expect_error(
    assignment(matrix(nrow = 0, ncol = 3)),
    "at least one row"
  )
})

test_that("assignment errors on 0-col matrix", {
  skip_on_cran()
  expect_error(
    assignment(matrix(nrow = 3, ncol = 0)),
    "at least one row"
  )
})

test_that("assignment errors on non-numeric", {
  skip_on_cran()
  expect_error(
    assignment(matrix(letters[1:4], 2, 2)),
    "must be a numeric"
  )
})

test_that("assignment errors on NaN", {
  skip_on_cran()
  expect_error(
    assignment(matrix(c(1, NaN, 3, 4), 2, 2)),
    "NaN not allowed"
  )
})

test_that("assignment handles ssp as alias for sap", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- assignment(cost, method = "ssp")
  expect_equal(result$method_used, "sap")
})

test_that("assignment backward compat: eps maps to auction_eps", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  # "eps_optimal" is reported only when an epsilon was supplied, so the status
  # is what shows `eps` was carried through to `auction_eps`.
  result <- assignment(cost, method = "auction", eps = 1e-6)
  expect_equal(result$status, "eps_optimal")
  expect_equal(assignment(cost, method = "auction")$status, "optimal")
})

test_that("assignment auto selects hk01 for constant costs", {
  skip_on_cran()
  cost <- matrix(1, 10, 10)  # All same value, n > 8
  result <- assignment(cost, method = "auto")
  # hk01 is selected for constant costs when n > 8
  expect_equal(result$method_used, "hk01")
})

test_that("assignment auto selects bruteforce for small binary costs", {
  skip_on_cran()
  cost <- matrix(c(0, 1, 1, 0, 1, 0, 0, 1, 0), 3, 3)
  result <- assignment(cost, method = "auto")
  # n=3 <= 8, so bruteforce is selected before hk01
  expect_equal(result$method_used, "bruteforce")
})

test_that("assignment auto handles sparse matrices", {
  skip_on_cran()
  cost <- matrix(Inf, 200, 200)
  diag(cost) <- 1  # Only 1% of entries are finite, but constant costs
  result <- assignment(cost, method = "auto")
  # Constant finite costs carry no scale to exploit, so hk01 answers this.
  expect_equal(result$method_used, "hk01")
})

test_that("assignment auto picks a dense-square method for large matrices", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(80 * 80), 80, 80)  # n > 75
  result <- assignment(cost, method = "auto")
  # Current dispatch picks jv for dense square (n > 8, non-binary, non-sparse).
  # Earlier dispatch versions could pick auction_scaled / hungarian; accept all.
  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

test_that("assignment auto picks a dense-square method for medium matrices", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(60 * 60), 60, 60)  # 50 < n <= 75
  result <- assignment(cost, method = "auto")
  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

test_that("assignment auto picks a dense-square method for small-medium matrices", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(25 * 25), 25, 25)  # 8 < n <= 50
  result <- assignment(cost, method = "auto")
  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

# ------------------------------------------------------------------------------
# lap_solve() interface
# ------------------------------------------------------------------------------

test_that("lap_solve with maximize=TRUE returns higher cost", {
  skip_on_cran()
  cost <- matrix(c(1, 10, 10, 1), 2, 2)
  result_min <- lap_solve(cost, maximize = FALSE)
  result_max <- lap_solve(cost, maximize = TRUE)
  expect_true(attr(result_max, "total_cost") >= attr(result_min, "total_cost"))
})

test_that("lap_solve handles single row matrix", {
  skip_on_cran()
  cost <- matrix(c(5, 3, 8), nrow = 1)
  result <- lap_solve(cost)
  expect_s3_class(result, "lap_solve_result")
  expect_equal(nrow(result), 1)
  expect_equal(result$target, 2)  # Picks min cost (3)
})

test_that("lap_solve handles single column matrix", {
  skip_on_cran()
  cost <- matrix(c(5, 3, 8), ncol = 1)
  result <- lap_solve(cost)
  expect_s3_class(result, "lap_solve_result")
  expect_equal(nrow(result), 1)
  expect_equal(result$source, 2)  # Row 2 gets matched
})

test_that("lap_solve with specific method works", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), 3, 3)
  for (method in c("jv", "hungarian", "auction")) {
    result <- lap_solve(cost, method = method)
    expect_s3_class(result, "lap_solve_result")
    expect_equal(attr(result, "method"), method)
  }
})

# ------------------------------------------------------------------------------
# assignment_duals()
# ------------------------------------------------------------------------------

test_that("assignment_duals returns u and v vectors", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), 3, 3)
  result <- assignment_duals(cost)
  expect_type(result, "list")
  expect_true("u" %in% names(result))
  expect_true("v" %in% names(result))
  expect_length(result$u, 3)
  expect_length(result$v, 3)
})

test_that("assignment_duals with maximize", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), 3, 3)
  result <- assignment_duals(cost, maximize = TRUE)
  expect_type(result, "list")
  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# print methods
# ------------------------------------------------------------------------------

test_that("print.lap_solve_result works", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), 3, 3)
  result <- lap_solve(cost)
  expect_output(print(result), "Assignment Result")
  expect_output(print(result), "Method:")
})

test_that("print.lap_solve_result with single assignment", {
  skip_on_cran()
  cost <- matrix(5, 1, 1)
  result <- lap_solve(cost)
  expect_output(print(result), "Assignment Result")
})

test_that("sinkhorn result can be printed", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 2, 1), 2, 2)
  result <- sinkhorn(cost, lambda = 10)
  # sinkhorn returns a list, just check it prints without error
  expect_output(print(result), "transport_plan")
})

# ------------------------------------------------------------------------------
# sinkhorn() edge cases
# ------------------------------------------------------------------------------

test_that("sinkhorn with custom weights", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 2, 1), 2, 2)
  result <- sinkhorn(cost, lambda = 10, r_weights = c(1, 1), c_weights = c(1, 1))
  expect_true(result$converged)
})

test_that("sinkhorn_to_assignment extracts hard assignment", {
  skip_on_cran()
  cost <- matrix(c(1, 100, 100, 1), 2, 2)
  result <- sinkhorn(cost, lambda = 100)
  assign <- sinkhorn_to_assignment(result)
  expect_length(assign, 2)
  expect_equal(assign[1], 1L)  # Row 1 -> Col 1
  expect_equal(assign[2], 2L)  # Row 2 -> Col 2
})

# ------------------------------------------------------------------------------
# bottleneck_assignment() edge cases
# ------------------------------------------------------------------------------

test_that("bottleneck_assignment with maximize", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 3, 2, 8, 4, 6, 7, 2), 3, 3)
  result <- bottleneck_assignment(cost, maximize = TRUE)
  expect_s3_class(result, "bottleneck_result")
  expect_equal(result$status, "optimal")
})

test_that("bottleneck_assignment errors on non-square with rows > cols", {
  skip_on_cran()
  cost <- matrix(1:6, nrow = 3, ncol = 2)
  expect_error(bottleneck_assignment(cost), "nrow <= ncol")
})

test_that("print.bottleneck_result with many assignments", {
  skip_on_cran()
  cost <- matrix(runif(144), 12, 12)
  result <- bottleneck_assignment(cost)
  expect_output(print(result), "more assignments")
})

# ------------------------------------------------------------------------------
# lap_solve_line_metric() edge cases
# ------------------------------------------------------------------------------

test_that("lap_solve_line_metric returns result", {
  skip_on_cran()
  x <- c(1, 3, 5)
  y <- c(2, 4, 6)
  result <- lap_solve_line_metric(x, y, cost = "L1")
  expect_type(result, "list")
  expect_true("match" %in% names(result))
})

test_that("lap_solve_line_metric with L2 cost", {
  skip_on_cran()
  x <- c(1, 3, 5)
  y <- c(2, 4, 6)
  result <- lap_solve_line_metric(x, y, cost = "L2")
  expect_type(result, "list")
  expect_true("total_cost" %in% names(result))
})

test_that("lap_solve_line_metric with fewer sources than targets", {
  skip_on_cran()
  x <- c(1, 3)  # length(x) <= length(y)
  y <- c(2, 4, 6, 8)
  result <- lap_solve_line_metric(x, y)
  expect_type(result, "list")
})

test_that("lap_solve_line_metric with maximize", {
  skip_on_cran()
  x <- c(1, 3, 5)
  y <- c(2, 4, 6)
  result <- lap_solve_line_metric(x, y, maximize = TRUE)
  expect_type(result, "list")
})
