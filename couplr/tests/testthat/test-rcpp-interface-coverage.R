# ==============================================================================
# Tests to increase rcpp_interface.cpp coverage through high-level functions
# ==============================================================================

# ------------------------------------------------------------------------------
# Greedy matching strategies
# ------------------------------------------------------------------------------

test_that("greedy_matching_sorted via greedy matching", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = 2:6)
  right <- data.frame(x = 6:10, y = 7:11)

  result <- match_couples(left, right, vars = c("x", "y"), strategy = "sorted", method = "greedy")

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})

test_that("greedy_matching_row_best via greedy matching", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = 2:6)
  right <- data.frame(x = 6:10, y = 7:11)

  result <- match_couples(left, right, vars = c("x", "y"), strategy = "row_best", method = "greedy")

  expect_s3_class(result, "matching_result")
})

test_that("greedy_matching_pq via greedy matching", {
  skip_on_cran()
  left <- data.frame(x = 1:5, y = 2:6)
  right <- data.frame(x = 6:10, y = 7:11)

  result <- match_couples(left, right, vars = c("x", "y"), strategy = "pq", method = "greedy")

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Auction solver variants via assignment()
# ------------------------------------------------------------------------------

test_that("auction solver variations", {
  skip_on_cran()
  cost <- matrix(runif(16), 4, 4)

  result1 <- assignment(cost, method = "auction")
  result2 <- assignment(cost, method = "auction_gs")
  result3 <- assignment(cost, method = "auction_scaled")

  expect_equal(result1$status, "optimal")
  expect_equal(result2$status, "optimal")
  expect_equal(result3$status, "optimal")
})

# ------------------------------------------------------------------------------
# K-best via lap_solve_kbest
# ------------------------------------------------------------------------------

test_that("lap_solve_kbest exercises murty", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- lap_solve_kbest(cost, k = 3)

  expect_s3_class(result, "lap_solve_kbest_result")
  expect_true(nrow(result) >= 1)
})

# ------------------------------------------------------------------------------
# Network simplex
# ------------------------------------------------------------------------------

test_that("network_simplex solver works on various inputs", {
  skip_on_cran()
  # Small
  cost1 <- matrix(c(1, 2, 3, 4), 2, 2)
  result1 <- assignment(cost1, method = "network_simplex")
  expect_equal(result1$status, "optimal")

  # Medium
  cost2 <- matrix(runif(36), 6, 6)
  result2 <- assignment(cost2, method = "network_simplex")
  expect_equal(result2$status, "optimal")

  # Rectangular
  cost3 <- matrix(runif(12), 3, 4)
  result3 <- assignment(cost3, method = "network_simplex")
  expect_equal(result3$status, "optimal")
})

# ------------------------------------------------------------------------------
# JV duals via assignment_duals()
# ------------------------------------------------------------------------------

test_that("assignment_duals returns dual variables", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment_duals(cost)

  expect_true("u" %in% names(result))
  expect_true("v" %in% names(result))
  expect_equal(length(result$u), 3)
  expect_equal(length(result$v), 3)
})

# ------------------------------------------------------------------------------
# Sinkhorn
# ------------------------------------------------------------------------------

test_that("sinkhorn and sinkhorn_to_assignment work together", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 3, 2, 4, 6, 7, 1, 2), 3, 3)

  sink_result <- sinkhorn(cost, lambda = 20)
  expect_true(is.matrix(sink_result$transport_plan))

  hard_match <- sinkhorn_to_assignment(sink_result)
  expect_equal(length(hard_match), 3)
})

# ------------------------------------------------------------------------------
# Bottleneck
# ------------------------------------------------------------------------------

test_that("bottleneck_assignment exercises bottleneck solver", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 3, 2, 4, 6, 7, 1, 2), 3, 3)

  result <- bottleneck_assignment(cost)

  expect_s3_class(result, "bottleneck_result")
  expect_true(result$bottleneck >= 0)
})

# ------------------------------------------------------------------------------
# Line metric
# ------------------------------------------------------------------------------

test_that("lap_solve_line_metric exercises line metric solver", {
  skip_on_cran()
  x <- c(1, 3, 5, 7)
  y <- c(2, 4, 6, 8)

  result <- lap_solve_line_metric(x, y)

  expect_equal(length(result$match), 4)
})

# ------------------------------------------------------------------------------
# Cycle cancel
# ------------------------------------------------------------------------------

test_that("cycle_cancel solver works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Push-relabel
# ------------------------------------------------------------------------------

test_that("push_relabel solver works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment(cost, method = "push_relabel")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Ramshaw-Tarjan
# ------------------------------------------------------------------------------

test_that("ramshaw_tarjan solver works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# CSA (dense-scan SSP)
# ------------------------------------------------------------------------------

test_that("csa solver works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Gabow-Tarjan
# ------------------------------------------------------------------------------

test_that("gabow_tarjan solver works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment(cost, method = "gabow_tarjan")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# HK01
# ------------------------------------------------------------------------------

test_that("hk01 solver works on binary costs", {
  skip_on_cran()
  cost <- matrix(c(0, 1, 1, 0, 1, 1, 0, 1, 1), 3, 3)

  result <- assignment(cost, method = "hk01")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# SSAP bucket
# ------------------------------------------------------------------------------

test_that("ssap_bucket solver works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment(cost, method = "ssap_bucket")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# LAPMOD (sparse)
# ------------------------------------------------------------------------------

test_that("lapmod solver works on sparse matrices", {
  skip_on_cran()
  # Create sparse matrix
  cost <- matrix(NA, 5, 5)
  diag(cost) <- 1:5

  result <- assignment(cost, method = "lapmod")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# CSFlow
# ------------------------------------------------------------------------------

test_that("csflow solver works", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- assignment(cost, method = "csflow")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Bruteforce
# ------------------------------------------------------------------------------

test_that("bruteforce solver works on tiny matrices", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- assignment(cost, method = "bruteforce")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Maximize mode
# ------------------------------------------------------------------------------

test_that("maximize mode works with various solvers", {
  skip_on_cran()
  cost <- matrix(c(1, 10, 10, 1), 2, 2)

  result_jv <- assignment(cost, method = "jv", maximize = TRUE)
  result_hung <- assignment(cost, method = "hungarian", maximize = TRUE)
  result_auction <- assignment(cost, method = "auction", maximize = TRUE)

  expect_true(result_jv$total_cost >= 10)
  expect_true(result_hung$total_cost >= 10)
  expect_true(result_auction$total_cost >= 10)
})
