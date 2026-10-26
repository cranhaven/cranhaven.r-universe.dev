# tests/testthat/test-assignment-csa.R
# Tests for Goldberg-Kennedy CSA (Cost-Scaling Assignment) solver

test_that("csa solves simple 3x3 problem correctly", {
  cost <- matrix(c(4, 2, 5,
                   3, 3, 6,
                   7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_equal(result$method_used, "csa")
  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))
  expect_equal(length(unique(result$match)), 3)  # All different columns

  # Check cost is correct (should be 9: row1->col2 (2) + row2->col1 (3) + row3->col3 (4))
  expect_equal(result$total_cost, 9)
})

test_that("csa handles rectangular matrix (n < m)", {
  cost <- matrix(c(1, 2, 3, 4,
                   5, 1, 2, 3,
                   4, 5, 1, 2), nrow = 3, byrow = TRUE)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 4))
  expect_equal(length(unique(result$match)), 3)

  # Optimal: row1->col1 (1) + row2->col2 (1) + row3->col3 (1) = 3
  expect_equal(result$total_cost, 3)
})

test_that("csa handles rectangular matrix (n > m)", {
  cost <- matrix(c(1, 5, 4,
                   2, 1, 5,
                   3, 2, 1,
                   4, 3, 2), nrow = 4, byrow = TRUE)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 4)
  # 3 columns available, so at least one row unmatched (0)
  expect_true(sum(result$match > 0) == 3)
})

test_that("csa handles NA (forbidden) entries", {
  cost <- matrix(c(NA, 2, 5,
                   3, NA, 6,
                   7, 5, NA), nrow = 3, byrow = TRUE)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  # Should avoid diagonal (NA entries)
  expect_true(result$match[1] != 1)
  expect_true(result$match[2] != 2)
  expect_true(result$match[3] != 3)
})

test_that("csa handles Inf (forbidden) entries", {
  cost <- matrix(c(Inf, 2, 5,
                   3, Inf, 6,
                   7, 5, Inf), nrow = 3, byrow = TRUE)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  # Should avoid diagonal (Inf entries)
  expect_true(result$match[1] != 1)
  expect_true(result$match[2] != 2)
  expect_true(result$match[3] != 3)
})

test_that("csa maximization works", {
  cost <- matrix(c(4, 2, 5,
                   3, 3, 6,
                   7, 5, 4), nrow = 3, byrow = TRUE)

  min_result <- assignment(cost, maximize = FALSE, method = "csa")
  max_result <- assignment(cost, maximize = TRUE, method = "csa")

  expect_lt(min_result$total_cost, max_result$total_cost)
  expect_equal(min_result$total_cost, 9)  # 2 + 3 + 4
  expect_equal(max_result$total_cost, 15) # 4 + 6 + 5 = 15 (row1->col1, row2->col3, row3->col2)
})

test_that("csa matches JV on random problems", {
  set.seed(123)
  for (trial in 1:10) {
    n <- sample(5:20, 1)
    m <- sample(n:(n + 5), 1)
    cost <- matrix(runif(n * m, 1, 100), nrow = n, ncol = m)

    csa_result <- assignment(cost, method = "csa")
    jv_result <- assignment(cost, method = "jv")

    expect_equal(csa_result$total_cost, jv_result$total_cost,
                 tolerance = 1e-9,
                 info = sprintf("Trial %d: n=%d, m=%d", trial, n, m))
  }
})

test_that("csa matches JV on random maximization problems", {
  set.seed(456)
  for (trial in 1:10) {
    n <- sample(5:15, 1)
    m <- sample(n:(n + 3), 1)
    cost <- matrix(runif(n * m, 1, 100), nrow = n, ncol = m)

    csa_result <- assignment(cost, method = "csa", maximize = TRUE)
    jv_result <- assignment(cost, method = "jv", maximize = TRUE)

    expect_equal(csa_result$total_cost, jv_result$total_cost,
                 tolerance = 1e-9,
                 info = sprintf("Trial %d: n=%d, m=%d", trial, n, m))
  }
})

test_that("csa matches JV on real-valued costs with fine differences", {
  # Values in [0, 1] at 1e-5 resolution give small optimal gaps -- the regime
  # where CSA's absolute epsilon termination could previously pick a suboptimal
  # assignment. The integer-scaling of non-integer costs keeps it exact; JV is
  # the ground-truth optimum.
  set.seed(20260718)
  for (trial in 1:12) {
    n <- sample(4:12, 1)
    cost <- matrix(round(runif(n * n, 0, 1), 5), nrow = n, ncol = n)
    csa <- assignment(cost, method = "csa")
    jv  <- assignment(cost, method = "jv")
    expect_equal(csa$total_cost, jv$total_cost, tolerance = 1e-9,
                 info = sprintf("trial %d n=%d", trial, n))
  }
})

test_that("csa handles sparse problems (many NA entries)", {
  set.seed(789)
  n <- 15
  cost <- matrix(NA_real_, nrow = n, ncol = n)

  # Fill in ~30% of entries, ensuring each row has at least 2 options
  for (i in 1:n) {
    cols <- sample(1:n, max(2, ceiling(n * 0.3)))
    for (j in cols) {
      cost[i, j] <- runif(1, 1, 100)
    }
  }

  csa_result <- assignment(cost, method = "csa")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(csa_result$status, "optimal")
  expect_equal(csa_result$total_cost, jv_result$total_cost, tolerance = 1e-9)

  # Verify no NA was chosen
  for (i in 1:n) {
    expect_false(is.na(cost[i, csa_result$match[i]]))
  }
})

test_that("csa handles uniform costs", {
  cost <- matrix(5, nrow = 4, ncol = 4)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_equal(result$total_cost, 20)  # 4 * 5
  expect_equal(length(unique(result$match)), 4)
})

test_that("csa handles negative costs", {
  cost <- matrix(c(-4, -2, -5,
                   -3, -3, -6,
                   -7, -5, -4), nrow = 3, byrow = TRUE)

  csa_result <- assignment(cost, method = "csa")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(csa_result$total_cost, jv_result$total_cost, tolerance = 1e-9)
})

test_that("csa handles large cost range", {
  cost <- matrix(c(1, 1000000, 500000,
                   1000000, 1, 500000,
                   500000, 500000, 1), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "csa")
  expect_equal(result$total_cost, 3)  # Optimal is diagonal
  expect_equal(result$match, c(1, 2, 3))
})

test_that("csa handles 2x2 problem", {
  # matrix(c(1,2,3,4), nrow=2) fills by column: [[1,3],[2,4]]
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 2)
  # Optimal: row1->col1 (1) + row2->col2 (4) = 5, or row1->col2 (3) + row2->col1 (2) = 5
  expect_equal(result$total_cost, 5)
})

test_that("csa handles 1x1 problem", {
  cost <- matrix(42, nrow = 1, ncol = 1)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_equal(result$match, 1)
  expect_equal(result$total_cost, 42)
})

test_that("csa handles very sparse connectivity", {
  # Each row has only 1 valid option
  cost <- matrix(NA_real_, nrow = 3, ncol = 3)
  cost[1, 2] <- 5
  cost[2, 3] <- 3
  cost[3, 1] <- 7

  result <- assignment(cost, method = "csa")
  expect_equal(result$status, "optimal")
  expect_equal(result$match, c(2, 3, 1))
  expect_equal(result$total_cost, 15)  # 5 + 3 + 7
})

test_that("csa handles fractional costs", {
  cost <- matrix(c(1.5, 2.7, 3.2,
                   4.1, 0.9, 2.8,
                   3.3, 4.5, 1.1), nrow = 3, byrow = TRUE)

  csa_result <- assignment(cost, method = "csa")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(csa_result$total_cost, jv_result$total_cost, tolerance = 1e-9)
})

test_that("csa stress test with medium-sized problem", {
  set.seed(2024)
  n <- 50
  cost <- matrix(runif(n * n, 0, 1000), nrow = n, ncol = n)

  csa_result <- assignment(cost, method = "csa")
  jv_result <- assignment(cost, method = "jv")

  expect_equal(csa_result$status, "optimal")
  expect_equal(csa_result$total_cost, jv_result$total_cost, tolerance = 1e-6)
})
