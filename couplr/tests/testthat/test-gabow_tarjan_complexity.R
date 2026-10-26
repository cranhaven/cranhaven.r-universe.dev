# test-gabow_tarjan_complexity.R
# Deterministic tests for Gabow-Tarjan algorithm correctness and behavior
# Replaces timing-based complexity tests which are inherently flaky in CI

test_that("Gabow-Tarjan produces optimal results across problem sizes", {
  skip_on_cran()

  # Test correctness across multiple problem sizes with fixed seeds
  test_cases <- list(
    list(n = 10, seed = 100),
    list(n = 20, seed = 200),
    list(n = 50, seed = 300),
    list(n = 80, seed = 400),
    list(n = 100, seed = 500)
  )

  for (tc in test_cases) {
    set.seed(tc$seed)
    cost <- matrix(sample(1:1000, tc$n * tc$n, replace = TRUE), nrow = tc$n, ncol = tc$n)

    # Solve with Gabow-Tarjan and reference solver
    res_gt <- lap_solve(cost, method = "gabow_tarjan")
    res_jv <- lap_solve(cost, method = "jv")

    # Must produce same optimal cost
    expect_equal(
      attr(res_gt, "total_cost"),
      attr(res_jv, "total_cost"),
      label = sprintf("Gabow-Tarjan vs JV optimal cost at n=%d", tc$n)
    )

    # Must have valid assignment
    expect_equal(nrow(res_gt), tc$n)
    expect_true(all(res_gt$source >= 1 & res_gt$source <= tc$n))
    expect_true(all(res_gt$target >= 1 & res_gt$target <= tc$n))
    expect_equal(length(unique(res_gt$target)), tc$n)  # All targets assigned exactly once
  }
})

test_that("Gabow-Tarjan handles uniform cost matrices", {
  skip_on_cran()

  # Uniform costs - all assignments have same cost
  # This tests tie-breaking behavior
  sizes <- c(10, 30, 50, 70)

  for (n in sizes) {
    cost <- matrix(1, nrow = n, ncol = n)

    res <- lap_solve(cost, method = "gabow_tarjan")

    expect_equal(attr(res, "total_cost"), n)  # n assignments × cost 1
    expect_equal(nrow(res), n)
    expect_equal(length(unique(res$target)), n)
  }
})

test_that("Gabow-Tarjan handles diagonal-dominant matrices", {
  skip_on_cran()

  # Diagonal is cheapest - optimal is identity matching
  for (n in c(10, 25, 50)) {
    cost <- matrix(100, nrow = n, ncol = n)
    diag(cost) <- 1

    res <- lap_solve(cost, method = "gabow_tarjan")

    expect_equal(attr(res, "total_cost"), n)  # n × 1
    # Should match diagonal
    expect_true(all(res$source == res$target))
  }
})

test_that("Gabow-Tarjan handles anti-diagonal matrices", {
  skip_on_cran()

  # Anti-diagonal is cheapest
  for (n in c(10, 25, 50)) {
    cost <- matrix(100, nrow = n, ncol = n)
    for (i in 1:n) {
      cost[i, n - i + 1] <- 1
    }

    res <- lap_solve(cost, method = "gabow_tarjan")

    expect_equal(attr(res, "total_cost"), n)
    # Should match anti-diagonal
    for (i in 1:n) {
      row_match <- res[res$source == i, "target"]
      expect_equal(as.integer(row_match), n - i + 1)
    }
  }
})

test_that("Gabow-Tarjan matches Hungarian on random instances", {
  skip_on_cran()

  # Multiple random instances with different characteristics
  test_configs <- list(
    list(n = 20, max_cost = 100, seed = 1001),
    list(n = 30, max_cost = 1000, seed = 1002),
    list(n = 40, max_cost = 10000, seed = 1003),
    list(n = 50, max_cost = 100, seed = 1004)
  )

  for (cfg in test_configs) {
    set.seed(cfg$seed)
    cost <- matrix(
      sample(1:cfg$max_cost, cfg$n * cfg$n, replace = TRUE),
      nrow = cfg$n, ncol = cfg$n
    )

    res_gt <- lap_solve(cost, method = "gabow_tarjan")
    res_hungarian <- lap_solve(cost, method = "hungarian")

    expect_equal(
      attr(res_gt, "total_cost"),
      attr(res_hungarian, "total_cost"),
      label = sprintf("GT vs Hungarian at n=%d, max_cost=%d", cfg$n, cfg$max_cost)
    )
  }
})

test_that("Gabow-Tarjan handles edge cases correctly", {
  skip_on_cran()

  # 1x1 matrix
  cost_1x1 <- matrix(42, nrow = 1, ncol = 1)
  res <- lap_solve(cost_1x1, method = "gabow_tarjan")
  expect_equal(attr(res, "total_cost"), 42)
  expect_equal(nrow(res), 1)

  # 2x2 matrix with unique optimum
  cost_2x2 <- matrix(c(1, 100, 100, 1), nrow = 2, byrow = TRUE)
  res <- lap_solve(cost_2x2, method = "gabow_tarjan")
  expect_equal(attr(res, "total_cost"), 2)  # Diagonal: 1 + 1

  # 3x3 with known optimum
  cost_3x3 <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
  ), nrow = 3, byrow = TRUE)
  res_gt <- lap_solve(cost_3x3, method = "gabow_tarjan")
  res_jv <- lap_solve(cost_3x3, method = "jv")
  expect_equal(attr(res_gt, "total_cost"), attr(res_jv, "total_cost"))

  # Large costs (test numeric stability)
  set.seed(999)
  cost_large <- matrix(sample(1e6:1e7, 25, replace = TRUE), nrow = 5, ncol = 5)
  res_gt <- lap_solve(cost_large, method = "gabow_tarjan")
  res_jv <- lap_solve(cost_large, method = "jv")
  expect_equal(attr(res_gt, "total_cost"), attr(res_jv, "total_cost"))

  # Zero costs
  cost_zero <- matrix(0, nrow = 5, ncol = 5)
  res <- lap_solve(cost_zero, method = "gabow_tarjan")
  expect_equal(attr(res, "total_cost"), 0)
})

test_that("Gabow-Tarjan handles sparse-like cost patterns", {
  skip_on_cran()

  # Matrix with most entries very high, few low
  # Simulates sparse assignment problems
  set.seed(777)
  n <- 30
  cost <- matrix(1000000, nrow = n, ncol = n)

  # Add n random low-cost entries ensuring feasibility
  for (i in 1:n) {
    j <- sample(1:n, 1)
    cost[i, j] <- sample(1:100, 1)
  }
  # Ensure at least one feasible assignment exists by adding diagonal fallback
  diag(cost) <- pmin(diag(cost), 500)

  res_gt <- lap_solve(cost, method = "gabow_tarjan")
  res_jv <- lap_solve(cost, method = "jv")

  expect_equal(
    attr(res_gt, "total_cost"),
    attr(res_jv, "total_cost")
  )
})

test_that("Gabow-Tarjan is deterministic", {
  skip_on_cran()

  # Same input should always produce same output
  set.seed(12345)
  cost <- matrix(sample(1:500, 400, replace = TRUE), nrow = 20, ncol = 20)

  results <- replicate(5, {
    res <- lap_solve(cost, method = "gabow_tarjan")
    list(
      cost = attr(res, "total_cost"),
      assignment = paste(res$target, collapse = ",")
    )
  }, simplify = FALSE)

  # All runs should produce identical results
  costs <- sapply(results, `[[`, "cost")
  assignments <- sapply(results, `[[`, "assignment")

  expect_equal(length(unique(costs)), 1)
  expect_equal(length(unique(assignments)), 1)
})
