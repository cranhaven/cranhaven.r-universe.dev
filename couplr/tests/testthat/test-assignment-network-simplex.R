# Test suite for Network Simplex solver
# Tests the network simplex algorithm for linear assignment

test_that("network_simplex solves basic 3x3 problem", {
  cost <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$total_cost, 15)
  expect_equal(result$status, "optimal")
  expect_equal(result$method_used, "network_simplex")
  expect_equal(length(result$match), 3)
})

test_that("network_simplex matches JV on random square matrices", {
  set.seed(123)

  for (n in c(3, 5, 8, 10, 15)) {
    cost <- matrix(runif(n * n, 1, 100), nrow = n)

    ns_result <- assignment(cost, method = "network_simplex")
    jv_result <- assignment(cost, method = "jv")

    expect_equal(ns_result$total_cost, jv_result$total_cost, tolerance = 1e-6,
                 info = paste("n =", n))
  }
})

test_that("network_simplex handles rectangular matrix (rows < cols)", {
  set.seed(42)
  cost <- matrix(runif(15), nrow = 3, ncol = 5)

  ns_result <- assignment(cost, method = "network_simplex")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(ns_result$total_cost, jv_result$total_cost, tolerance = 1e-6)
  expect_equal(length(ns_result$match), 3)
})

test_that("network_simplex handles rectangular matrix (rows > cols) via transpose", {
  set.seed(42)
  cost <- matrix(runif(15), nrow = 5, ncol = 3)

  ns_result <- assignment(cost, method = "network_simplex")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(ns_result$total_cost, jv_result$total_cost, tolerance = 1e-6)
  # Match vector has length nrow (5), but only min(5,3)=3 are matched (non-zero)
  expect_equal(length(ns_result$match), 5)
  expect_equal(sum(ns_result$match > 0), 3)
})

test_that("network_simplex handles maximize", {
  set.seed(123)
  cost <- matrix(runif(25), nrow = 5)

  ns_min <- assignment(cost, method = "network_simplex", maximize = FALSE)
  ns_max <- assignment(cost, method = "network_simplex", maximize = TRUE)
  jv_max <- assignment(cost, method = "jv", maximize = TRUE)

  # Maximization should give larger cost than minimization
  expect_gt(ns_max$total_cost, ns_min$total_cost)

  # Should match JV for maximization
  expect_equal(ns_max$total_cost, jv_max$total_cost, tolerance = 1e-6)
})

test_that("network_simplex works with multiple random seeds", {
  passed <- 0

  for (seed in 1:20) {
    set.seed(seed)
    cost <- matrix(runif(64), nrow = 8)

    ns_result <- assignment(cost, method = "network_simplex")
    jv_result <- assignment(cost, method = "jv")

    if (abs(ns_result$total_cost - jv_result$total_cost) < 1e-6) {
      passed <- passed + 1
    }
  }

  expect_equal(passed, 20)
})

test_that("network_simplex works with integer costs", {
  cost <- matrix(c(1L, 4L, 7L,
                   2L, 5L, 8L,
                   3L, 6L, 9L), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "network_simplex")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(result$total_cost, jv_result$total_cost)
})

test_that("network_simplex handles uniform costs", {
  cost <- matrix(5, nrow = 4, ncol = 4)

  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$total_cost, 20)  # 4 * 5
  expect_equal(result$status, "optimal")
})
