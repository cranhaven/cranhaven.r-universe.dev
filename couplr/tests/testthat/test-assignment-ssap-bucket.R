# test-assignment-ssap-bucket.R
# Tests for SSAP Bucket (Dial's algorithm) solver

test_that("ssap_bucket works on basic 3x3 problem", {
  cost <- matrix(c(
    4, 2, 5,
    3, 3, 6,
    7, 5, 4
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))
  expect_true(all(table(result$match) == 1))  # All unique
  expect_type(result$total_cost, "double")
  expect_true(is.finite(result$total_cost))
})

test_that("ssap_bucket gives same result as JV", {
  cost <- matrix(c(
    10, 5, 13,
    7, 12, 9,
    8, 6, 11
  ), nrow = 3, byrow = TRUE)

  result_ssap <- assignment(cost, maximize = FALSE, method = "ssap_bucket")
  result_jv <- assignment(cost, method = "jv")

  expect_equal(result_ssap$total_cost, result_jv$total_cost)
})

test_that("ssap_bucket handles maximization", {
  cost <- matrix(c(
    10, 5, 13,
    7, 12, 9,
    8, 6, 11
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = TRUE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))
  expect_true(all(table(result$match) == 1))

  # Compare with JV maximization
  result_jv <- assignment(cost, method = "jv", maximize = TRUE)
  expect_equal(result$total_cost, result_jv$total_cost)
})

test_that("ssap_bucket handles integer costs efficiently", {
  # This is ideal for bucket algorithm
  cost <- matrix(c(
    10, 20, 30,
    15, 25, 35,
    12, 18, 24
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost)
})

test_that("ssap_bucket handles decimal costs that scale nicely", {
  # Costs that are multiples of 0.1
  cost <- matrix(c(
    1.5, 2.3, 3.7,
    2.1, 1.9, 2.8,
    3.2, 2.4, 1.6
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost, tolerance = 1e-10)
})

test_that("ssap_bucket handles small integer range", {
  # Small integer costs (0-10) - ideal for bucket algorithm
  cost <- matrix(c(
    4, 2, 8,
    6, 5, 3,
    7, 9, 1
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)

  # Verify optimality
  result_hungarian <- assignment(cost, method = "hungarian")
  expect_equal(result$total_cost, result_hungarian$total_cost)
})

test_that("ssap_bucket handles rectangular matrices", {
  # More columns than rows
  cost <- matrix(c(
    10, 5, 13, 8, 12,
    7, 12, 9, 15, 6,
    8, 6, 11, 7, 10
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 5))
  expect_true(all(table(result$match) == 1))

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost)
})

test_that("ssap_bucket handles NA as forbidden edges", {
  cost <- matrix(c(
    4, 2, 5,
    3, NA, 6,
    7, 5, 4
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))

  # Row 2 should not use column 2 (NA)
  expect_true(result$match[2] != 2)
})

test_that("ssap_bucket handles Inf as forbidden edges", {
  cost <- matrix(c(
    4, 2, 5,
    3, 3, 6,
    7, 5, Inf
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)

  # Row 3 should not use column 3 (Inf)
  expect_true(result$match[3] != 3)
})

test_that("ssap_bucket handles negative costs", {
  cost <- matrix(c(
    -5, -2, -8,
    -3, -7, -1,
    -9, -4, -6
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))
  expect_true(result$total_cost < 0)

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost, tolerance = 1e-10)
})

test_that("ssap_bucket handles mixed positive and negative costs", {
  cost <- matrix(c(
    10, -5, 13,
    -7, 12, 9,
    8, -6, 11
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 3))

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost, tolerance = 1e-10)
})

test_that("ssap_bucket works on larger problem", {
  set.seed(42)
  n <- 20
  m <- 25
  # Integer costs for efficiency
  cost <- matrix(sample(1:100, n*m, replace = TRUE), nrow = n)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), n)
  expect_true(all(result$match >= 1 & result$match <= m))
  expect_true(all(table(result$match) == 1))

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost, tolerance = 1e-8)
})

test_that("ssap_bucket handles all-equal costs", {
  n <- 4
  cost <- matrix(5, nrow = n, ncol = n)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  # Any matching is optimal
  expect_equal(length(result$match), n)
  expect_equal(result$total_cost, 5 * n)
})

test_that("ssap_bucket handles identity-like matrix", {
  # Diagonal should be optimal
  n <- 5
  cost <- matrix(100, nrow = n, ncol = n)
  diag(cost) <- 1

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(result$match, 1:n)
  expect_equal(result$total_cost, n)
})

test_that("ssap_bucket returns proper result structure", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3)
  result <- assignment(cost, method = "ssap_bucket")

  expect_true(is.list(result))
  expect_named(result,
               c("match", "total_cost", "status", "method_used", "dispatch",
                 "cardinality", "n_matched", "unmatched"),
               ignore.order = TRUE)
  expect_equal(result$method_used, "ssap_bucket")
  expect_s3_class(result, "lap_solve_result")
})

test_that("ssap_bucket handles very small costs", {
  cost <- matrix(c(
    0.001, 0.002, 0.005,
    0.003, 0.001, 0.006,
    0.007, 0.004, 0.001
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost, tolerance = 1e-10)
})

test_that("ssap_bucket handles zero costs", {
  cost <- matrix(c(
    0, 5, 10,
    5, 0, 15,
    10, 15, 0
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)
  expect_equal(result$total_cost, 0)
})

test_that("ssap_bucket empty matrix handling", {
  cost <- matrix(numeric(0), nrow = 0, ncol = 0)

  # Directly call C++ function to bypass R validation
  result <- lap_solve_ssap_bucket(cost, FALSE)

  expect_equal(length(result$match), 0)
  expect_equal(result$total_cost, 0)
})

test_that("ssap_bucket infeasible problem detection", {
  # All entries in row 2 are NA
  cost <- matrix(c(
    1, 2, 3,
    NA, NA, NA,
    4, 5, 6
  ), nrow = 3, byrow = TRUE)

  expect_error(
    assignment(cost, method = "ssap_bucket"),
    "no valid edges"
  )
})

test_that("ssap_bucket matches JV on random integer problems", {
  set.seed(123)

  for (n in 3:8) {
    for (m in n:(n+3)) {
      cost <- matrix(sample(1:50, n*m, replace = TRUE), nrow = n)

      result_ssap <- assignment(cost, method = "ssap_bucket")
      result_jv <- assignment(cost, method = "jv")

      expect_equal(result_ssap$total_cost, result_jv$total_cost,
                   tolerance = 1e-8,
                   info = sprintf("Failed at n=%d, m=%d", n, m))
    }
  }
})

test_that("ssap_bucket is efficient on large integer range", {
  # Bucket algorithm should handle this well
  cost <- matrix(c(
    1000, 5, 2000,
    10, 500, 3000,
    1500, 750, 25
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)

  # Verify optimality
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost)
})

test_that("ssap_bucket handles costs with different scales", {
  cost <- matrix(c(
    1.5, 2.5, 3.5,
    2.0, 1.0, 3.0,
    3.0, 2.0, 1.5
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, maximize = FALSE, method = "ssap_bucket")

  expect_equal(length(result$match), 3)

  # Verify optimality
  result_hungarian <- assignment(cost, method = "hungarian")
  expect_equal(result$total_cost, result_hungarian$total_cost, tolerance = 1e-10)
})

test_that("ssap_bucket solves exactly-scalable six-decimal costs (#19)", {
  # Every entry is a multiple of 1e-6, so the extended power-of-ten search
  # reaches scale 1e6 instead of rounding at 1e3 and flipping the optimum.
  cost <- matrix(c(
    0.142857, 0.285714, 0.428571,
    0.571428, 0.714285, 0.857142,
    0.999999, 0.111111, 0.222222
  ), nrow = 3, byrow = TRUE)

  result <- assignment(cost, method = "ssap_bucket")
  result_jv <- assignment(cost, method = "jv")
  expect_equal(result$total_cost, result_jv$total_cost, tolerance = 1e-9)
})

test_that("ssap_bucket rejects costs it cannot integer-scale exactly (#19)", {
  # Full-precision reals admit no bounded 10^-k scaling; the solver must refuse
  # rather than silently round and return a wrong optimum.
  set.seed(19)
  cost <- matrix(runif(9), nrow = 3)
  # Either rejection path (precision or magnitude) is correct; both refuse and
  # redirect rather than round to a wrong optimum.
  expect_error(
    assignment(cost, method = "ssap_bucket"),
    "jv' or 'auction'"
  )
})

test_that("ssap_bucket never returns a wrong optimum on fractional costs (#19)", {
  # Regression fuzz for the silent-rounding bug: for each random fractional
  # matrix, ssap_bucket must either error or match jv's exact optimum.
  set.seed(2192)
  for (trial in seq_len(400)) {
    n <- sample(2:5, 1)
    m <- n + sample(0:1, 1)
    maximize <- (trial %% 2L == 0L)
    if (trial %% 2L == 0L) {
      cost <- matrix(round(runif(n * m, 0, 3), 6), nrow = n)  # exactly 6-decimal
    } else {
      cost <- matrix(runif(n * m, 0, 3), nrow = n)            # full precision
    }

    got <- tryCatch(
      assignment(cost, method = "ssap_bucket", maximize = maximize),
      error = function(e) e
    )
    if (inherits(got, "error")) next  # refusing is allowed; rounding wrong is not

    truth <- assignment(cost, method = "jv", maximize = maximize)
    expect_equal(got$total_cost, truth$total_cost, tolerance = 1e-9,
                 info = sprintf("trial=%d n=%d m=%d maximize=%s",
                                trial, n, m, maximize))
  }
})
