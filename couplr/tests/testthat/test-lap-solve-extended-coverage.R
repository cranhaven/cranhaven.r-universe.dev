# ==============================================================================
# Extended coverage tests for lap_solve.R
# ==============================================================================

# ------------------------------------------------------------------------------
# assignment() auto-selection edge cases
# ------------------------------------------------------------------------------

test_that("assignment auto picks a dense-square method for 50x50", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(50 * 50), 50, 50)
  result <- assignment(cost, method = "auto")
  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

test_that("assignment auto picks a dense-square method for 60x60", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(60 * 60), 60, 60)
  result <- assignment(cost, method = "auto")
  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

test_that("assignment auto picks a dense-square method for 100x100", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(100 * 100), 100, 100)
  result <- assignment(cost, method = "auto")
  expect_true(result$method_used %in% c("jv", "auction_scaled", "hungarian"))
})

test_that("assignment auto sends a very rectangular matrix to jv", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(10 * 40), 10, 40)
  result <- assignment(cost, method = "auto")
  expect_equal(result$method_used, "jv")
})

test_that("assignment auto sends a large sparse matrix to jv", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(150 * 150), 150, 150)
  # Make >50% forbidden
  cost[sample(length(cost), length(cost) * 0.6)] <- Inf
  result <- assignment(cost, method = "auto")
  expect_equal(result$method_used, "jv")
})

test_that("assignment auto sends a small sparse matrix to jv", {
  skip_on_cran()
  set.seed(123)
  cost <- matrix(runif(50 * 50), 50, 50)
  # Make >50% forbidden
  cost[sample(length(cost), length(cost) * 0.6)] <- Inf
  result <- assignment(cost, method = "auto")
  expect_equal(result$method_used, "jv")
})

test_that("assignment auto selects hk01 for binary costs (large matrix)", {
  skip_on_cran()
  # Need n > 8 to avoid bruteforce
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

test_that("assignment handles eps parameter for backward compatibility", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  # A caller-supplied epsilon stops the auction short of the terminal epsilon
  # that makes it exact, so the status reports the bound rather than optimality.
  # That makes the status itself the evidence that `eps` reached `auction_eps`:
  # it stays "optimal" when no epsilon is supplied.
  result <- assignment(cost, method = "auction", eps = 1e-6)
  expect_equal(result$status, "eps_optimal")
  expect_equal(assignment(cost, method = "auction")$status, "optimal")
})

# ------------------------------------------------------------------------------
# bottleneck_assignment edge cases
# ------------------------------------------------------------------------------

test_that("print.bottleneck_result handles small result", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- bottleneck_assignment(cost)

  expect_output(print(result), "Bottleneck Assignment Result")
  expect_output(print(result), "Row 1 -> Column")
  expect_output(print(result), "Bottleneck value:")
})

test_that("print.bottleneck_result handles large result", {
  skip_on_cran()
  cost <- matrix(runif(15 * 15), 15, 15)
  result <- bottleneck_assignment(cost)

  # Should truncate after 10 assignments
  expect_output(print(result), "more assignments")
})

test_that("bottleneck_assignment returns invisible result", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- bottleneck_assignment(cost)

  expect_invisible(print(result))
})

# ------------------------------------------------------------------------------
# sinkhorn edge cases
# ------------------------------------------------------------------------------

test_that("sinkhorn handles non-matrix input", {
  skip_on_cran()
  cost <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  result <- sinkhorn(matrix(cost, 3, 3))
  expect_true(result$converged)
})

test_that("sinkhorn errors on non-numeric cost", {
  skip_on_cran()
  cost <- matrix(as.character(1:9), 3, 3)
  expect_error(sinkhorn(cost), "numeric")
})

test_that("sinkhorn errors on non-positive lambda", {
  skip_on_cran()
  cost <- matrix(1:9, 3, 3)
  expect_error(sinkhorn(cost, lambda = 0), "positive")
  expect_error(sinkhorn(cost, lambda = -1), "positive")
})

test_that("sinkhorn with custom marginals", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- sinkhorn(cost, r_weights = c(0.5, 0.3, 0.2), c_weights = c(0.4, 0.4, 0.2))
  expect_true(result$converged)
})

test_that("sinkhorn_to_assignment with result object", {
  skip_on_cran()
  cost <- matrix(c(1, 10, 10, 1), 2, 2)
  result <- sinkhorn(cost, lambda = 50)
  match <- sinkhorn_to_assignment(result)
  expect_length(match, 2)
  expect_true(all(match %in% 1:2))
})

test_that("sinkhorn_to_assignment with matrix input", {
  skip_on_cran()
  # Transport plan matrix directly
  plan <- matrix(c(0.9, 0.1, 0.1, 0.9), 2, 2)
  match <- sinkhorn_to_assignment(plan)
  expect_length(match, 2)
})

# ------------------------------------------------------------------------------
# assignment_duals edge cases
# ------------------------------------------------------------------------------

test_that("assignment_duals returns duals", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1, 3, 2, 4, 3, 2), 3, 3)

  result <- assignment_duals(cost)
  expect_true(!is.null(result$u))
  expect_true(!is.null(result$v))
  expect_equal(result$status, "optimal")
})

test_that("assignment_duals with maximization", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1, 3, 2, 4, 3, 2), 3, 3)
  result <- assignment_duals(cost, maximize = TRUE)
  expect_true(!is.null(result$u))
  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# lap_solve tidy interface
# ------------------------------------------------------------------------------

test_that("lap_solve returns tibble with correct structure", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)

  expect_s3_class(result, "lap_solve_result")
  expect_true(tibble::is_tibble(result))
  expect_true("source" %in% names(result))
  expect_true("target" %in% names(result))
  expect_true("cost" %in% names(result))
})

test_that("lap_solve with row/col names", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  rownames(cost) <- c("A", "B")
  colnames(cost) <- c("X", "Y")

  result <- lap_solve(cost)

  # Should have source and target columns
  expect_true(all(c("source", "target") %in% names(result)))
})

test_that("lap_solve preserves attributes", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- lap_solve(cost)

  expect_true(!is.null(attr(result, "total_cost")))
  expect_true(!is.null(attr(result, "method_used")))
})

# ------------------------------------------------------------------------------
# Error handling in assignment()
# ------------------------------------------------------------------------------

test_that("assignment errors on NaN values", {
  skip_on_cran()
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)
  expect_error(assignment(cost), "NaN")
})

test_that("assignment errors on empty matrix", {
  skip_on_cran()
  cost <- matrix(numeric(0), 0, 0)
  expect_error(assignment(cost), "at least one row")
})

test_that("assignment errors on non-numeric matrix", {
  skip_on_cran()
  cost <- matrix(letters[1:4], 2, 2)
  expect_error(assignment(cost), "numeric")
})

# ------------------------------------------------------------------------------
# Method-specific tests
# ------------------------------------------------------------------------------

test_that("ssp method aliases to sap", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- assignment(cost, method = "ssp")
  expect_equal(result$method_used, "sap")
})

test_that("all methods produce valid results on simple matrix", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 3, 2, 1, 4, 3, 2, 1), 3, 3)

  # hk01 requires binary costs, test separately
  methods <- c("jv", "hungarian", "auction", "auction_gs", "auction_scaled",
               "sap", "csflow", "bruteforce", "ssap_bucket",
               "cycle_cancel", "gabow_tarjan", "lapmod", "csa",
               "ramshaw_tarjan", "push_relabel", "sap_dense", "network_simplex")

  for (m in methods) {
    result <- assignment(cost, method = m)
    expect_equal(result$status, "optimal", info = paste("Method:", m))
    expect_length(result$match, 3)
  }
})

test_that("hk01 works on binary cost matrix", {
  skip_on_cran()
  cost <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  result <- assignment(cost, method = "hk01")
  expect_equal(result$status, "optimal")
  expect_length(result$match, 3)
})
