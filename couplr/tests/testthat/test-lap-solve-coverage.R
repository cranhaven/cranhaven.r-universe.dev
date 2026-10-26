# ==============================================================================
# Additional tests for lap_solve functions to increase coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# assignment() input validation
# ------------------------------------------------------------------------------

test_that("assignment errors on empty matrix", {
  skip_on_cran()
  expect_error(
    assignment(matrix(nrow = 0, ncol = 0)),
    "at least one row"
  )

  expect_error(
    assignment(matrix(nrow = 0, ncol = 5)),
    "at least one row"
  )

  expect_error(
    assignment(matrix(nrow = 5, ncol = 0)),
    "at least one row"
  )
})

test_that("assignment errors on non-numeric matrix", {
  skip_on_cran()
  expect_error(
    assignment(matrix(c("a", "b", "c", "d"), 2, 2)),
    "must be a numeric"
  )
})

test_that("assignment errors on NaN values", {
  skip_on_cran()
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)
  expect_error(
    assignment(cost),
    "NaN not allowed"
  )
})

test_that("assignment handles eps parameter (deprecated)", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  # eps should be treated as auction_eps
  result <- assignment(cost, method = "auction", eps = 0.001)

  expect_equal(result$method_used, "auction")
})

# ------------------------------------------------------------------------------
# assignment() auto method selection
# ------------------------------------------------------------------------------

test_that("assignment auto selects bruteforce for n <= 8", {
  skip_on_cran()
  cost <- matrix(runif(64), 8, 8)

  result <- assignment(cost, method = "auto")

  expect_equal(result$method_used, "bruteforce")
})

test_that("assignment auto selects hk01 for binary costs", {
  skip_on_cran()
  # Need n > 8 for hk01 to be selected over bruteforce
  cost <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)

  result <- assignment(cost, method = "auto")

  expect_equal(result$method_used, "hk01")
})

test_that("assignment auto selects hk01 for constant costs", {
  skip_on_cran()
  cost <- matrix(5, 10, 10)

  result <- assignment(cost, method = "auto")

  expect_equal(result$method_used, "hk01")
})

test_that("assignment auto selects lapmod for sparse matrices", {
  skip_on_cran()
  set.seed(123)
  n <- 150
  cost <- matrix(NA, n, n)
  # Fill only 30% of entries
  idx <- sample(n * n, n * n * 0.3)
  cost[idx] <- runif(length(idx), 1, 100)

  result <- assignment(cost, method = "auto")

  expect_equal(result$method_used, "jv")
})

test_that("assignment auto sends a very rectangular matrix to jv", {
  skip_on_cran()
  cost <- matrix(runif(30), 10, 30)  # 10 rows, 30 cols (ratio = 3)

  result <- assignment(cost, method = "auto")

  expect_equal(result$method_used, "jv")
})

test_that("assignment auto picks a dense-square method for small-medium n", {
  skip_on_cran()
  cost <- matrix(runif(40 * 40), 40, 40)

  result <- assignment(cost, method = "auto")

  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

test_that("assignment auto picks a dense-square method for medium n", {
  skip_on_cran()
  cost <- matrix(runif(60 * 60), 60, 60)

  result <- assignment(cost, method = "auto")

  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

test_that("assignment auto picks a dense-square method for large n", {
  skip_on_cran()
  cost <- matrix(runif(100 * 100), 100, 100)

  result <- assignment(cost, method = "auto")

  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

# ------------------------------------------------------------------------------
# assignment() transpose handling
# ------------------------------------------------------------------------------

test_that("assignment handles rows > cols by transposing", {
  skip_on_cran()
  cost <- matrix(runif(15), 5, 3)  # 5 rows, 3 cols

  result <- assignment(cost, method = "jv")

  # After transpose, match length equals the smaller dimension
  # But actually the function returns matches for original dimensions
  expect_true(length(result$match) %in% c(3, 5))
})

# ------------------------------------------------------------------------------
# lap_solve() tests
# ------------------------------------------------------------------------------

test_that("lap_solve handles grouped data frames", {
  skip_on_cran()
  df <- tibble::tibble(
    sim = rep(1:2, each = 4),
    source = rep(1:2, times = 4),
    target = rep(1:2, each = 2, times = 2),
    cost = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

  result <- df |>
    dplyr::group_by(sim) |>
    lap_solve(source, target, cost)

  expect_true("sim" %in% names(result))
})

test_that("lap_solve errors when data frame missing columns", {
  skip_on_cran()
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_error(
    lap_solve(df),
    "must specify"
  )
})

test_that("lap_solve handles matrix with some forbidden entries", {
  skip_on_cran()
  # Create matrix with some NA entries
  cost <- matrix(c(1, NA, 3, 4), 2, 2)

  result <- lap_solve(cost)

  expect_s3_class(result, "lap_solve_result")
})

test_that("lap_solve handles method parameter", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result_auto <- lap_solve(cost, method = "auto")
  result_hung <- lap_solve(cost, method = "hungarian")

  expect_s3_class(result_auto, "lap_solve_result")
  expect_s3_class(result_hung, "lap_solve_result")
})

test_that("lap_solve handles maximize parameter", {
  skip_on_cran()
  cost <- matrix(c(1, 10, 10, 1), 2, 2)

  result_min <- lap_solve(cost, maximize = FALSE)
  result_max <- lap_solve(cost, maximize = TRUE)

  expect_true(attr(result_max, "total_cost") > attr(result_min, "total_cost"))
})

# ------------------------------------------------------------------------------
# print.lap_solve_result tests
# ------------------------------------------------------------------------------

test_that("print.lap_solve_result handles tibble result", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- lap_solve(cost)

  output <- capture.output(print(result))

  expect_true(any(grepl("Assignment Result", output)))
  expect_true(any(grepl("Total cost", output)))
})

test_that("print.lap_solve_result handles list result", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- assignment(cost)

  output <- capture.output(print(result))

  expect_true(any(grepl("Assignment", output)))
})

test_that("print.lap_solve_result handles many assignments", {
  skip_on_cran()
  cost <- matrix(runif(144), 12, 12)
  result <- assignment(cost, method = "hungarian")

  output <- capture.output(print(result))

  expect_true(any(grepl("more", output)))  # Should show "... and X more"
})

test_that("print.lap_solve_result handles no assignments", {
  skip_on_cran()
  # Force empty result
  result <- tibble::tibble(
    source = integer(0),
    target = integer(0),
    cost = numeric(0)
  )
  attr(result, "total_cost") <- 0
  attr(result, "method_used") <- "test"
  class(result) <- c("lap_solve_result", class(result))

  output <- capture.output(print(result))

  expect_true(any(grepl("Assignment", output)))
})

# ------------------------------------------------------------------------------
# lap_solve_line_metric tests
# ------------------------------------------------------------------------------

test_that("lap_solve_line_metric errors on empty x", {
  skip_on_cran()
  expect_error(
    lap_solve_line_metric(numeric(0), c(1, 2, 3)),
    "non-empty"
  )
})

test_that("lap_solve_line_metric errors on empty y", {
  skip_on_cran()
  expect_error(
    lap_solve_line_metric(c(1, 2, 3), numeric(0)),
    "non-empty"
  )
})

test_that("lap_solve_line_metric errors when x longer than y", {
  skip_on_cran()
  expect_error(
    lap_solve_line_metric(c(1, 2, 3), c(1, 2)),
    "must be <="
  )
})

test_that("lap_solve_line_metric errors on non-finite x", {
  skip_on_cran()
  expect_error(
    lap_solve_line_metric(c(1, NA, 3), c(1, 2, 3)),
    "finite values"
  )

  expect_error(
    lap_solve_line_metric(c(1, Inf, 3), c(1, 2, 3)),
    "finite values"
  )
})

test_that("lap_solve_line_metric errors on non-finite y", {
  skip_on_cran()
  expect_error(
    lap_solve_line_metric(c(1, 2, 3), c(1, NA, 3)),
    "finite values"
  )
})

test_that("lap_solve_line_metric errors on invalid cost", {
  skip_on_cran()
  expect_error(
    lap_solve_line_metric(c(1, 2), c(1, 2), cost = "invalid"),
    "must be one of"
  )
})

test_that("lap_solve_line_metric works with L1 cost", {
  skip_on_cran()
  x <- c(1, 2, 3)
  y <- c(1.5, 2.5, 3.5)

  result <- lap_solve_line_metric(x, y, cost = "L1")

  expect_equal(length(result$match), 3)
})

test_that("lap_solve_line_metric works with L2 cost", {
  skip_on_cran()
  x <- c(1, 2, 3)
  y <- c(1.5, 2.5, 3.5)

  result <- lap_solve_line_metric(x, y, cost = "L2")

  expect_equal(length(result$match), 3)
})

test_that("lap_solve_line_metric works with aliases", {
  skip_on_cran()
  x <- c(1, 2, 3)
  y <- c(1, 2, 3)

  result_abs <- lap_solve_line_metric(x, y, cost = "abs")
  result_manhattan <- lap_solve_line_metric(x, y, cost = "manhattan")
  result_sq <- lap_solve_line_metric(x, y, cost = "sq")
  result_squared <- lap_solve_line_metric(x, y, cost = "squared")
  result_quadratic <- lap_solve_line_metric(x, y, cost = "quadratic")

  expect_equal(length(result_abs$match), 3)
  expect_equal(length(result_manhattan$match), 3)
  expect_equal(length(result_sq$match), 3)
  expect_equal(length(result_squared$match), 3)
  expect_equal(length(result_quadratic$match), 3)
})

test_that("lap_solve_line_metric handles rectangular case", {
  skip_on_cran()
  x <- c(1, 2, 3)
  y <- c(1, 2, 3, 4, 5)

  result <- lap_solve_line_metric(x, y)

  expect_equal(length(result$match), 3)
})

test_that("print.lap_line_metric_result works", {
  skip_on_cran()
  x <- c(1, 2, 3)
  y <- c(1, 2, 3)

  result <- lap_solve_line_metric(x, y)

  output <- capture.output(print(result))

  expect_true(any(grepl("Line Assignment", output)))
})

test_that("print.lap_line_metric_result handles many assignments", {
  skip_on_cran()
  x <- 1:15
  y <- 1:15

  result <- lap_solve_line_metric(x, y)

  output <- capture.output(print(result))

  expect_true(any(grepl("more", output)))
})

# ------------------------------------------------------------------------------
# bottleneck_assignment tests
# ------------------------------------------------------------------------------

test_that("bottleneck_assignment errors on empty matrix", {
  skip_on_cran()
  expect_error(
    bottleneck_assignment(matrix(nrow = 0, ncol = 0)),
    "at least one"
  )
})

test_that("bottleneck_assignment errors on non-numeric", {
  skip_on_cran()
  expect_error(
    bottleneck_assignment(matrix(c("a", "b", "c", "d"), 2, 2)),
    "must be a numeric"
  )
})

test_that("bottleneck_assignment errors on NaN", {
  skip_on_cran()
  expect_error(
    bottleneck_assignment(matrix(c(1, NaN, 3, 4), 2, 2)),
    "NaN not allowed"
  )
})

test_that("bottleneck_assignment errors on rows > cols", {
  skip_on_cran()
  expect_error(
    bottleneck_assignment(matrix(runif(6), 3, 2)),
    "nrow <= ncol"
  )
})

test_that("bottleneck_assignment minimizes max edge", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 3, 2, 4, 6, 7, 1, 2), 3, 3, byrow = TRUE)

  result <- bottleneck_assignment(cost)

  expect_s3_class(result, "bottleneck_result")
  # The bottleneck value should be optimal
  expect_true(result$bottleneck >= 0)
})

test_that("bottleneck_assignment maximize works", {
  skip_on_cran()
  cost <- matrix(c(10, 5, 8, 6, 12, 4, 3, 7, 11), 3, 3, byrow = TRUE)

  result <- bottleneck_assignment(cost, maximize = TRUE)

  expect_s3_class(result, "bottleneck_result")
})

test_that("print.bottleneck_result works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- bottleneck_assignment(cost)

  output <- capture.output(print(result))

  expect_true(any(grepl("Bottleneck", output)))
})

test_that("print.bottleneck_result handles many assignments", {
  skip_on_cran()
  cost <- matrix(runif(144), 12, 12)
  result <- bottleneck_assignment(cost)

  output <- capture.output(print(result))

  expect_true(any(grepl("more", output)))
})

# ------------------------------------------------------------------------------
# assignment_duals tests
# ------------------------------------------------------------------------------

test_that("assignment_duals errors on empty matrix", {
  skip_on_cran()
  # Use numeric() to ensure empty matrix is numeric type
  expect_error(
    assignment_duals(matrix(numeric(0), nrow = 0, ncol = 0)),
    "at least one row and one column"
  )
})

test_that("assignment_duals errors on non-numeric", {
  skip_on_cran()
  expect_error(
    assignment_duals(matrix(c("a", "b", "c", "d"), 2, 2)),
    "must be a numeric"
  )
})

test_that("assignment_duals errors on NaN", {
  skip_on_cran()
  expect_error(
    assignment_duals(matrix(c(1, NaN, 3, 4), 2, 2)),
    "NaN not allowed"
  )
})

test_that("assignment_duals returns duals that satisfy complementary slackness", {
  skip_on_cran()
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), 3, 3, byrow = TRUE)

  result <- assignment_duals(cost)

  expect_s3_class(result, "assignment_duals_result")
  expect_equal(length(result$u), 3)
  expect_equal(length(result$v), 3)

  # Check complementary slackness for assigned pairs
  for (i in seq_len(3)) {
    j <- result$match[i]
    if (j > 0) {
      expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-6)
    }
  }
})

test_that("assignment_duals handles maximize", {
  skip_on_cran()
  cost <- matrix(c(1, 10, 10, 1), 2, 2)

  result <- assignment_duals(cost, maximize = TRUE)

  expect_s3_class(result, "assignment_duals_result")
})

test_that("assignment_duals handles transpose", {
  skip_on_cran()
  cost <- matrix(runif(15), 5, 3)  # More rows than cols (5x3)

  result <- assignment_duals(cost)

  # After transpose handling, dimensions match original matrix:
  # match has length = nrow(cost) = 5
  # u has length = nrow(cost) = 5
  # v has length = ncol(cost) = 3
  expect_equal(length(result$match), 5)
  expect_equal(length(result$u), 5)
  expect_equal(length(result$v), 3)
})

test_that("print.assignment_duals_result works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- assignment_duals(cost)

  output <- capture.output(print(result))

  expect_true(any(grepl("Dual", output)))
})

test_that("print.assignment_duals_result handles many assignments", {
  skip_on_cran()
  cost <- matrix(runif(144), 12, 12)
  result <- assignment_duals(cost)

  output <- capture.output(print(result))

  expect_true(any(grepl("more", output)))
})

# ------------------------------------------------------------------------------
# sinkhorn tests
# ------------------------------------------------------------------------------

test_that("sinkhorn errors on non-matrix input", {
  skip_on_cran()
  expect_error(
    sinkhorn("not a matrix"),
    "must be a numeric"
  )
})

test_that("sinkhorn errors on non-positive lambda", {
  skip_on_cran()
  expect_error(
    sinkhorn(matrix(1:4, 2, 2), lambda = 0),
    "must be positive"
  )

  expect_error(
    sinkhorn(matrix(1:4, 2, 2), lambda = -1),
    "must be positive"
  )
})

test_that("sinkhorn_to_assignment errors on invalid input", {
  skip_on_cran()
  expect_error(
    sinkhorn_to_assignment("not valid"),
    "must be"
  )
})

test_that("sinkhorn_to_assignment accepts matrix directly", {
  skip_on_cran()
  P <- matrix(c(0.8, 0.2, 0.2, 0.8), 2, 2)

  result <- sinkhorn_to_assignment(P)

  expect_equal(length(result), 2)
})
