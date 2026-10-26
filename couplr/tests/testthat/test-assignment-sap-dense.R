# tests/testthat/test-assignment-sap_dense.R
# Tests for the dense-scan successive shortest path solver via assignment()

test_that("sap_dense method works via assignment()", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "sap_dense")

  expect_equal(result$status, "optimal")
  expect_equal(result$method_used, "sap_dense")
  expect_equal(length(result$match), 3)
  expect_true(all(result$match > 0))  # All assigned
  expect_equal(length(unique(result$match)), 3)  # All different
})

test_that("sap_dense matches JV on small problems", {
  set.seed(123)
  for (n in c(3, 5, 8)) {
    cost <- matrix(runif(n * n, 1, 100), nrow = n)

    sap_dense_res <- assignment(cost, method = "sap_dense")
    jv_result <- assignment(cost, method = "jv")

    expect_equal(sap_dense_res$total_cost, jv_result$total_cost,
                 tolerance = 1e-6,
                 label = paste("n =", n))
  }
})

test_that("sap_dense handles rectangular matrices", {
  # 3x5 matrix (more columns than rows)
  cost <- matrix(c(1, 2, 3, 4, 5,
                   6, 7, 8, 9, 10,
                   11, 12, 13, 14, 15), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "sap_dense")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match > 0))
  expect_equal(length(unique(result$match)), 3)
})

test_that("sap_dense handles transposed matrices (rows > cols)", {
  # 5x3 matrix (auto-transposed internally)
  cost <- matrix(c(1, 2, 3,
                   4, 5, 6,
                   7, 8, 9,
                   10, 11, 12,
                   13, 14, 15), nrow = 5, byrow = TRUE)

  result <- assignment(cost, method = "sap_dense")

  # Result should have 5 entries (one per row), but only 3 can be matched
  expect_equal(length(result$match), 5)
  matched <- sum(result$match > 0)
  expect_equal(matched, 3)  # Can only match 3
})

test_that("sap_dense handles maximize = TRUE", {
  cost <- matrix(c(10, 5, 8, 20, 3, 15, 7, 12, 6), nrow = 3, byrow = TRUE)

  min_result <- assignment(cost, method = "sap_dense", maximize = FALSE)
  max_result <- assignment(cost, method = "sap_dense", maximize = TRUE)

  expect_gt(max_result$total_cost, min_result$total_cost)
})

test_that("sap_dense handles forbidden edges (Inf)", {
  cost <- matrix(c(1, Inf, 3,
                   4, 5, Inf,
                   Inf, 8, 9), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "sap_dense")

  expect_equal(result$status, "optimal")

  # Check no forbidden edges were assigned
  for (i in 1:3) {
    j <- result$match[i]
    if (j > 0) {
      expect_true(is.finite(cost[i, j]))
    }
  }
})

test_that("sap_dense handles NA as forbidden", {
  cost <- matrix(c(1, NA, 3,
                   4, 5, NA,
                   NA, 8, 9), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "sap_dense")

  expect_equal(result$status, "optimal")
})

test_that("sap_dense scales to medium problems", {
  skip_on_cran()

  set.seed(42)
  n <- 50
  cost <- matrix(runif(n * n, 1, 100), nrow = n)

  result <- assignment(cost, method = "sap_dense")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(result$total_cost, jv_result$total_cost, tolerance = 1e-4)
})
