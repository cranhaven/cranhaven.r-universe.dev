# ==============================================================================
# Tests for additional LAP solvers
# ==============================================================================
# Tests for solvers that need more coverage

# Helper function to create test problems
make_test_cost <- function(n = 5, seed = 123) {
  set.seed(seed)
  matrix(runif(n * n), n, n)
}

# ------------------------------------------------------------------------------
# Gabow-Tarjan solver tests
# ------------------------------------------------------------------------------

test_that("gabow_tarjan solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "gabow_tarjan")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
  expect_equal(result$status, "optimal")
})

test_that("gabow_tarjan handles larger problems", {
  cost <- make_test_cost(10)

  result <- assignment(cost, method = "gabow_tarjan")

  expect_equal(length(result$match), 10)
  expect_equal(sort(result$match), 1:10)
})

test_that("gabow_tarjan handles maximization", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "gabow_tarjan", maximize = TRUE)

  expect_equal(result$total_cost, 10)
})

test_that("gabow_tarjan handles NA entries", {
  cost <- matrix(c(1, NA, NA, 1), 2, 2)

  result <- assignment(cost, method = "gabow_tarjan")

  expect_equal(result$total_cost, 2)
})

# ------------------------------------------------------------------------------
# Cycle canceling solver tests
# ------------------------------------------------------------------------------

test_that("cycle_cancel solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "cycle_cancel")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("cycle_cancel handles larger problems", {
  cost <- make_test_cost(8)

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(length(result$match), 8)
})

test_that("cycle_cancel handles maximization", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "cycle_cancel", maximize = TRUE)

  expect_equal(result$total_cost, 10)
})

# ------------------------------------------------------------------------------
# Network simplex solver tests
# ------------------------------------------------------------------------------

test_that("network_simplex solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "network_simplex")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("network_simplex handles larger problems", {
  cost <- make_test_cost(15)

  result <- assignment(cost, method = "network_simplex")

  expect_equal(length(result$match), 15)
  expect_equal(sort(result$match), 1:15)
})

test_that("network_simplex handles rectangular matrix", {
  cost <- matrix(runif(12), 3, 4)

  result <- assignment(cost, method = "network_simplex")

  expect_equal(length(result$match), 3)
})

# ------------------------------------------------------------------------------
# Push-relabel solver tests
# ------------------------------------------------------------------------------

test_that("push_relabel solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "push_relabel")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("push_relabel handles larger problems", {
  cost <- make_test_cost(12)

  result <- assignment(cost, method = "push_relabel")

  expect_equal(length(result$match), 12)
})

# ------------------------------------------------------------------------------
# dense-scan SSP solver tests
# ------------------------------------------------------------------------------

test_that("sap_dense solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "sap_dense")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("sap_dense handles larger problems", {
  cost <- make_test_cost(10)

  result <- assignment(cost, method = "sap_dense")

  expect_equal(length(result$match), 10)
})

# ------------------------------------------------------------------------------
# Ramshaw-Tarjan solver tests
# ------------------------------------------------------------------------------

test_that("ramshaw_tarjan solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "ramshaw_tarjan")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("ramshaw_tarjan handles rectangular matrices well", {
  # Ramshaw-Tarjan is optimized for rectangular
  cost <- matrix(runif(20), 4, 5)

  result <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(length(result$match), 4)
})

# ------------------------------------------------------------------------------
# LAPMOD solver tests (sparse JV variant)
# ------------------------------------------------------------------------------

test_that("lapmod solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "lapmod")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("lapmod handles sparse matrices efficiently", {
  # Create sparse matrix (many Inf entries)
  cost <- matrix(Inf, 10, 10)
  diag(cost) <- runif(10)
  cost[1, 2] <- 0.5
  cost[2, 1] <- 0.5

  result <- assignment(cost, method = "lapmod")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# SSAP bucket solver tests
# ------------------------------------------------------------------------------

test_that("ssap_bucket solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "ssap_bucket")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("ssap_bucket handles integer costs", {
  cost <- matrix(c(1L, 5L, 5L, 1L), 2, 2)

  result <- assignment(cost, method = "ssap_bucket")

  expect_equal(result$total_cost, 2)
})

# ------------------------------------------------------------------------------
# CS-Flow solver tests
# ------------------------------------------------------------------------------

test_that("csflow solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "csflow")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("csflow handles larger problems", {
  cost <- make_test_cost(8)

  result <- assignment(cost, method = "csflow")

  expect_equal(length(result$match), 8)
})

# ------------------------------------------------------------------------------
# SAP/SSP solver tests
# ------------------------------------------------------------------------------

test_that("sap solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "sap")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2)
})

test_that("ssp is alias for sap", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "ssp")

  expect_equal(result$total_cost, 2)
})

# ------------------------------------------------------------------------------
# Auction solver variants tests
# ------------------------------------------------------------------------------

test_that("auction solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "auction")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2, tolerance = 1e-6)
})

test_that("auction_gs solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "auction_gs")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2, tolerance = 1e-6)
})

test_that("auction_scaled solves basic problem", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- assignment(cost, method = "auction_scaled")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 2, tolerance = 1e-6)
})

test_that("auction handles larger problems", {
  cost <- make_test_cost(20)

  result <- assignment(cost, method = "auction_scaled")

  expect_equal(length(result$match), 20)
  expect_equal(sort(result$match), 1:20)
})

# ------------------------------------------------------------------------------
# HK01 solver tests (binary costs)
# ------------------------------------------------------------------------------

test_that("hk01 solves binary cost problem", {
  cost <- matrix(c(0, 1, 1, 0), 2, 2)

  result <- assignment(cost, method = "hk01")

  expect_s3_class(result, "lap_solve_result")
  expect_equal(result$total_cost, 0)
})

test_that("hk01 handles all-ones matrix", {
  cost <- matrix(1, 3, 3)

  result <- assignment(cost, method = "hk01")

  expect_equal(result$total_cost, 3)
})

# ------------------------------------------------------------------------------
# Cross-solver consistency tests
# ------------------------------------------------------------------------------

test_that("all solvers produce same optimal cost", {
  set.seed(999)
  cost <- matrix(runif(16), 4, 4)

  methods <- c("hungarian", "jv", "auction_scaled", "sap", "gabow_tarjan")

  results <- lapply(methods, function(m) {
    assignment(cost, method = m)$total_cost
  })

  # All should be equal (within tolerance)
  for (i in 2:length(results)) {
    expect_equal(results[[1]], results[[i]], tolerance = 1e-6,
                 info = paste("Comparing", methods[1], "vs", methods[i]))
  }
})

# ------------------------------------------------------------------------------
# Line metric solver tests
# ------------------------------------------------------------------------------

test_that("line_metric solver works", {
  result <- lap_solve_line_metric(
    x = c(1, 3, 5),
    y = c(2, 4, 6),
    cost = "L1"
  )

  expect_s3_class(result, "lap_line_metric_result")
  expect_equal(length(result$match), 3)
})

test_that("line_metric handles L2 cost", {
  result <- lap_solve_line_metric(
    x = c(1, 3, 5),
    y = c(2, 4, 6),
    cost = "L2"
  )

  expect_s3_class(result, "lap_line_metric_result")
})

# ------------------------------------------------------------------------------
# K-best solver tests (Murty)
# ------------------------------------------------------------------------------

test_that("lap_solve_kbest finds k best solutions", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- lap_solve_kbest(cost, k = 2)

  expect_s3_class(result, "data.frame")
  # Should have 2 solutions (2x2 has 2! = 2 possible assignments)
  expect_equal(length(unique(result$solution_id)), 2)
})

test_that("lap_kbest_lawler direct call works", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- lap_kbest_lawler(cost, k = 2)

  expect_type(result, "list")
})

# ------------------------------------------------------------------------------
# prepare_cost_matrix tests
# ------------------------------------------------------------------------------

test_that("lap_prepare_cost_matrix works", {
  cost <- matrix(c(1, NA, 3, 4), 2, 2)

  result <- lap_prepare_cost_matrix(cost, FALSE)

  expect_type(result, "list")
  expect_true("cost" %in% names(result))
  expect_true("mask" %in% names(result))
})
