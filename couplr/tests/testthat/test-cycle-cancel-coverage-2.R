# ==============================================================================
# More tests for cycle_cancel solver coverage - triggering cycle cancellation
# ==============================================================================

test_that("cycle_cancel handles transposed matrix (more rows than cols)", {
  skip_on_cran()
  # 5x3 matrix - more rows than columns triggers transpose
  cost <- matrix(runif(15), 5, 3)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(length(result$match), 5)
  expect_equal(result$status, "optimal")
  # Only 3 rows can be matched to 3 columns
  expect_equal(sum(result$match > 0), 3)
})

test_that("cycle_cancel handles large rectangular matrix", {
  skip_on_cran()
  cost <- matrix(runif(40), 8, 5)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(length(result$match), 8)
})

test_that("cycle_cancel handles matrix with negative costs", {
  skip_on_cran()
  cost <- matrix(c(-5, -1, -2, -10, -3, -8, -4, -6, -7), 3, 3)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles maximize = TRUE", {
  skip_on_cran()
  cost <- matrix(c(1, 100, 100, 1, 50, 50, 50, 50, 25), 3, 3)

  result_min <- assignment(cost, method = "cycle_cancel", maximize = FALSE)
  result_max <- assignment(cost, method = "cycle_cancel", maximize = TRUE)

  expect_true(result_max$total_cost >= result_min$total_cost)
})

test_that("cycle_cancel handles matrix with many forbidden edges", {
  skip_on_cran()
  cost <- matrix(Inf, 4, 4)
  # Create a sparse pattern
  cost[1, 1] <- 1
  cost[2, 2] <- 2
  cost[3, 3] <- 3
  cost[4, 4] <- 4
  cost[1, 2] <- 5
  cost[2, 1] <- 5

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles 6x6 matrix", {
  skip_on_cran()
  set.seed(42)
  cost <- matrix(runif(36), 6, 6)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(length(result$match), 6)
  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles cost matrix that might trigger cycles", {
  skip_on_cran()
  # A matrix where initial SSP solution might have negative cycles
  cost <- matrix(c(
    1, 9, 2, 8,
    8, 2, 9, 1,
    3, 7, 4, 6,
    6, 4, 7, 3
  ), 4, 4, byrow = TRUE)

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 4)
})

test_that("cycle_cancel handles matrix with uniform costs except diagonal", {
  skip_on_cran()
  # This pattern can create interesting cycle structures
  cost <- matrix(10, 4, 4)
  diag(cost) <- 1

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
  expect_equal(result$total_cost, 4)  # 4 * 1 = 4
})

test_that("cycle_cancel handles 2x5 rectangular (more cols)", {
  skip_on_cran()
  cost <- matrix(1:10, 2, 5)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(length(result$match), 2)
  expect_equal(sum(result$match > 0), 2)
})

test_that("cycle_cancel handles matrix with zero costs", {
  skip_on_cran()
  cost <- matrix(0, 3, 3)
  cost[1, 2] <- 1
  cost[2, 3] <- 1
  cost[3, 1] <- 1

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel correctly matches jv on various sizes", {
  skip_on_cran()
  set.seed(123)
  for (n in c(3, 5, 7, 10)) {
    cost <- matrix(runif(n * n), n, n)

    result_cc <- assignment(cost, method = "cycle_cancel")
    result_jv <- assignment(cost, method = "jv")

    expect_equal(result_cc$total_cost, result_jv$total_cost,
                 tolerance = 1e-9, info = sprintf("n=%d", n))
  }
})

test_that("cycle_cancel handles single element sparse pattern", {
  skip_on_cran()
  # Only one valid assignment per row/col
  cost <- matrix(Inf, 3, 3)
  cost[1, 2] <- 5
  cost[2, 3] <- 3
  cost[3, 1] <- 7

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
  expect_equal(result$total_cost, 15)  # 5 + 3 + 7
})
