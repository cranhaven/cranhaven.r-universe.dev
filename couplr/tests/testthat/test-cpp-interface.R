# ==============================================================================
# Tests for C++ interface functions (rcpp_interface.cpp exports)
# ==============================================================================

# These tests cover the exported C++ functions that wrap LAP solvers

# ------------------------------------------------------------------------------
# lap_prepare_cost_matrix tests
# ------------------------------------------------------------------------------

test_that("lap_prepare_cost_matrix prepares matrix for minimization", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- lap_prepare_cost_matrix(cost, maximize = FALSE)

  expect_type(result, "list")
  expect_true("cost" %in% names(result))
})

test_that("lap_prepare_cost_matrix prepares matrix for maximization", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- lap_prepare_cost_matrix(cost, maximize = TRUE)

  expect_type(result, "list")
})

test_that("lap_prepare_cost_matrix handles NA values", {
  cost <- matrix(c(1, NA, 3, 4), 2, 2)

  result <- lap_prepare_cost_matrix(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_jv tests
# ------------------------------------------------------------------------------

test_that("lap_solve_jv solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_jv(cost, maximize = FALSE)

  expect_type(result, "list")
  expect_equal(result$match, c(1L, 2L))
  # The core computes the potentials whether or not anyone asks, and the one
  # entry point hands them back.
  expect_length(result$u, 2L)
  expect_length(result$v, 2L)
})

test_that("lap_solve_jv handles maximization", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_jv(cost, maximize = TRUE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_auction tests
# ------------------------------------------------------------------------------

test_that("lap_solve_auction solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_auction(cost, maximize = FALSE)

  expect_type(result, "list")
})

test_that("lap_solve_auction accepts custom epsilon", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_auction(cost, maximize = FALSE, eps = 0.01)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_auction_scaled tests
# ------------------------------------------------------------------------------

test_that("lap_solve_auction_scaled with alpha7 schedule", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_auction_scaled(cost, maximize = FALSE, schedule = "alpha7")

  expect_type(result, "list")
})

test_that("lap_solve_auction_scaled with pow2 schedule", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_auction_scaled(cost, maximize = FALSE, schedule = "pow2")

  expect_type(result, "list")
})

test_that("lap_solve_auction_scaled with halves schedule", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_auction_scaled(cost, maximize = FALSE, schedule = "halves")

  expect_type(result, "list")
})

test_that("lap_solve_auction_scaled errors on invalid schedule", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  expect_error(
    lap_solve_auction_scaled(cost, maximize = FALSE, schedule = "invalid"),
    "Invalid schedule"
  )
})

# ------------------------------------------------------------------------------
# lap_solve_auction_gauss_seidel tests
# ------------------------------------------------------------------------------

test_that("lap_solve_auction_gs (Gauss-Seidel) solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  # This uses the assignment wrapper with auction_gs method
  result <- assignment(cost, method = "auction_gs")

  expect_s3_class(result, "lap_solve_result")
})

# ------------------------------------------------------------------------------
# lap_solve_ssp tests
# ------------------------------------------------------------------------------

test_that("lap_solve_ssp solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_ssp(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_hungarian tests
# ------------------------------------------------------------------------------

test_that("lap_solve_hungarian solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_hungarian(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_csflow tests
# ------------------------------------------------------------------------------

test_that("lap_solve_csflow solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_csflow(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_hk01 tests
# ------------------------------------------------------------------------------

test_that("lap_solve_hk01 solves binary problem", {
  cost <- matrix(c(0, 1, 1, 0), 2, 2)

  result <- lap_solve_hk01(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_ssap_bucket tests
# ------------------------------------------------------------------------------

test_that("lap_solve_ssap_bucket solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_ssap_bucket(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_cycle_cancel tests
# ------------------------------------------------------------------------------

test_that("lap_solve_cycle_cancel solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_cycle_cancel(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_gabow_tarjan tests
# ------------------------------------------------------------------------------

test_that("lap_solve_gabow_tarjan solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_lapmod tests
# ------------------------------------------------------------------------------

test_that("lap_solve_lapmod solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_lapmod(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_bottleneck tests
# ------------------------------------------------------------------------------

test_that("lap_solve_bottleneck solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_bottleneck(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_csa tests
# ------------------------------------------------------------------------------

test_that("lap_solve_csa solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_csa(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_ramshaw_tarjan tests
# ------------------------------------------------------------------------------

test_that("lap_solve_ramshaw_tarjan solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_ramshaw_tarjan(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_push_relabel tests
# ------------------------------------------------------------------------------

test_that("lap_solve_push_relabel solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_push_relabel(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_kbest_murty tests
# ------------------------------------------------------------------------------

test_that("lap_kbest_murty finds k best solutions", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- lap_kbest_murty(cost, k = 2, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_sinkhorn tests
# ------------------------------------------------------------------------------

test_that("lap_solve_sinkhorn computes transport plan", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- lap_solve_sinkhorn(cost, lambda = 1.0, tol = 1e-6, max_iter = 1000)

  expect_type(result, "list")
})

test_that("lap_solve_sinkhorn with custom weights", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  r_weights <- c(0.5, 0.5)
  c_weights <- c(0.5, 0.5)

  result <- lap_solve_sinkhorn(cost, lambda = 1.0, tol = 1e-6, max_iter = 1000,
                               r_weights = r_weights, c_weights = c_weights)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# sinkhorn_round tests
# ------------------------------------------------------------------------------

test_that("sinkhorn_round converts transport plan to assignment", {
  # Create a doubly-stochastic-like matrix
  P <- matrix(c(0.6, 0.2, 0.2, 0.4, 0.4, 0.2, 0.0, 0.4, 0.6), 3, 3)

  result <- sinkhorn_round(P)

  expect_type(result, "integer")
  expect_length(result, 3)
})

# ------------------------------------------------------------------------------
# lap_solve_line_metric tests
# ------------------------------------------------------------------------------

test_that("lap_solve_line_metric with L1 cost", {
  x <- c(1, 3, 5)
  y <- c(2, 4, 6)

  result <- lap_solve_line_metric(x, y, cost = "L1")

  expect_type(result, "list")
})

test_that("lap_solve_line_metric with L2 cost", {
  x <- c(1, 3, 5)
  y <- c(2, 4, 6)

  result <- lap_solve_line_metric(x, y, cost = "L2")

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_network_simplex tests
# ------------------------------------------------------------------------------

test_that("lap_solve_network_simplex solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- lap_solve_network_simplex(cost)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# greedy_matching tests
# ------------------------------------------------------------------------------

test_that("greedy_matching with row_best strategy", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- greedy_matching(cost, maximize = FALSE, strategy = "row_best")

  expect_type(result, "list")
})

test_that("greedy_matching with sorted strategy", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- greedy_matching(cost, maximize = FALSE, strategy = "sorted")

  expect_type(result, "list")
})

test_that("greedy_matching with pq strategy", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- greedy_matching(cost, maximize = FALSE, strategy = "pq")

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# lap_solve_bruteforce tests
# ------------------------------------------------------------------------------

test_that("lap_solve_bruteforce solves small problem", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- lap_solve_bruteforce(cost, maximize = FALSE)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# Consistency across C++ solvers
# ------------------------------------------------------------------------------

test_that("different C++ solvers produce same optimal cost", {
  set.seed(42)
  cost <- matrix(runif(16), 4, 4)

  result_jv <- lap_solve_jv(cost, maximize = FALSE)
  result_hungarian <- lap_solve_hungarian(cost, maximize = FALSE)
  result_ssp <- lap_solve_ssp(cost, maximize = FALSE)

  # Extract total costs
  get_cost <- function(res) {
    if (!is.null(res$total_cost)) return(res$total_cost)
    if (!is.null(res$cost)) return(res$cost)
    return(NA)
  }

  cost_jv <- get_cost(result_jv)
  cost_hun <- get_cost(result_hungarian)
  cost_ssp <- get_cost(result_ssp)

  # If costs are available, they should match
  if (!is.na(cost_jv) && !is.na(cost_hun)) {
    expect_equal(cost_jv, cost_hun, tolerance = 1e-6)
  }
  if (!is.na(cost_jv) && !is.na(cost_ssp)) {
    expect_equal(cost_jv, cost_ssp, tolerance = 1e-6)
  }
})
