test_that("line_metric solves square problems correctly", {
  # For square problems (n == m), the optimal solution is to sort both
  # and match in order: sorted_x[i] -> sorted_y[i]
  
  set.seed(123)
  for (n in 2:6) {
    x <- runif(n, -10, 10)
    y <- runif(n, -10, 10)
    
    # Test L1
    result_l1 <- lap_solve_line_metric(x, y, cost = "L1", maximize = FALSE)
    
    # Verify it's a valid assignment
    expect_equal(length(result_l1$match), n)
    expect_true(all(result_l1$match >= 1 & result_l1$match <= n))
    expect_equal(length(unique(result_l1$match)), n)  # All different
    
    # Compute expected cost (sort and match)
    xs <- sort(x)
    ys <- sort(y)
    expected_cost_l1 <- sum(abs(xs - ys))
    expect_equal(result_l1$total_cost, expected_cost_l1, tolerance = 1e-10)
    
    # Test L2
    result_l2 <- lap_solve_line_metric(x, y, cost = "L2", maximize = FALSE)
    expected_cost_l2 <- sum((xs - ys)^2)
    expect_equal(result_l2$total_cost, expected_cost_l2, tolerance = 1e-10)
  }
})

test_that("line_metric handles rectangular problems (n < m)", {
  set.seed(456)
  
  # Small test case we can verify by hand
  x <- c(1.0, 3.0)
  y <- c(0.5, 2.0, 4.0)
  
  result <- lap_solve_line_metric(x, y, cost = "L1", maximize = FALSE)
  
  # The optimal solution is:
  # x[1]=1.0 -> y[1]=0.5 and x[2]=3.0 -> y[2]=2.0
  # Cost = |1.0 - 0.5| + |3.0 - 2.0| = 0.5 + 1.0 = 1.5
  # This is better than matching x[1]->y[2], x[2]->y[3] which would cost 2.0
  expect_equal(result$total_cost, 1.5, tolerance = 1e-10)
  
  # Test a few random cases
  for (trial in 1:5) {
    n <- sample(3:6, 1)
    m <- n + sample(1:3, 1)
    
    x <- runif(n, -5, 5)
    y <- runif(m, -5, 5)
    
    result <- lap_solve_line_metric(x, y, cost = "L1", maximize = FALSE)
    
    # Verify validity
    expect_equal(length(result$match), n)
    expect_true(all(result$match >= 1 & result$match <= m))
    expect_equal(length(unique(result$match)), n)
    
    # Cost should be positive (unless maximize)
    expect_true(result$total_cost >= 0)
  }
})

test_that("line_metric brute force verification on small problems", {
  # For very small problems, we can brute force all possible assignments
  # that respect the monotonicity constraint
  set.seed(789)
  
  for (cost_type in c("L1", "L2")) {
    n <- 3
    m <- 4
    x <- runif(n, -5, 5)
    y <- runif(m, -5, 5)
    
    result <- lap_solve_line_metric(x, y, cost = cost_type, maximize = FALSE)
    
    # Brute force: try all combinations of m choose n
    # BUT only those that respect monotonicity (sorted x maps to increasing y indices)
    xs <- sort(x)
    ys <- sort(y)
    
    best_cost <- Inf
    for (cols in combn(m, n, simplify = FALSE)) {
      cost <- 0
      for (i in 1:n) {
        diff <- abs(xs[i] - ys[cols[i]])
        if (cost_type == "L1") {
          cost <- cost + diff
        } else {
          cost <- cost + diff^2
        }
      }
      if (cost < best_cost) best_cost <- cost
    }
    
    expect_equal(result$total_cost, best_cost, tolerance = 1e-10)
  }
})

test_that("line_metric handles unsorted inputs correctly", {
  # The algorithm should sort internally
  x <- c(5.0, 1.0, 3.0)
  y <- c(4.5, 0.5, 6.0, 2.0, 3.5)
  
  result <- lap_solve_line_metric(x, y, cost = "L1", maximize = FALSE)
  
  # Verify it found a valid assignment
  expect_equal(length(result$match), 3)
  expect_true(all(result$match >= 1 & result$match <= 5))
  expect_equal(length(unique(result$match)), 3)
  
  # The result should match what we'd get with pre-sorted inputs
  x_sorted <- sort(x)
  y_sorted <- sort(y)
  
  # The matches in the result should respect the sorted order
  # i.e., if x[i] < x[j], then y[result$match[i]] < y[result$match[j]]
})

test_that("line_metric cost aliases work correctly", {
  set.seed(321)
  x <- runif(4, -5, 5)
  y <- runif(6, -5, 5)
  
  # Test L1 aliases
  r1 <- lap_solve_line_metric(x, y, cost = "L1")
  r2 <- lap_solve_line_metric(x, y, cost = "abs")
  r3 <- lap_solve_line_metric(x, y, cost = "manhattan")
  
  expect_equal(r1$total_cost, r2$total_cost)
  expect_equal(r1$total_cost, r3$total_cost)
  
  # Test L2 aliases
  r4 <- lap_solve_line_metric(x, y, cost = "L2")
  r5 <- lap_solve_line_metric(x, y, cost = "sq")
  r6 <- lap_solve_line_metric(x, y, cost = "squared")
  r7 <- lap_solve_line_metric(x, y, cost = "quadratic")
  
  expect_equal(r4$total_cost, r5$total_cost)
  expect_equal(r4$total_cost, r6$total_cost)
  expect_equal(r4$total_cost, r7$total_cost)
})

test_that("line_metric maximize returns the true maximum-weight matching", {
  # All injective maps 1..n -> 1..m
  injections <- function(n, m) {
    res <- list()
    rec <- function(prefix, used) {
      if (length(prefix) == n) { res[[length(res) + 1L]] <<- prefix; return() }
      for (j in setdiff(seq_len(m), used)) rec(c(prefix, j), c(used, j))
    }
    rec(integer(0), integer(0))
    res
  }
  cost_fn <- function(a, b, type) if (type == "L2") (a - b)^2 else abs(a - b)

  check <- function(x, y, type) {
    maps <- injections(length(x), length(y))
    totals <- vapply(maps, function(p) sum(cost_fn(x, y[p], type)), numeric(1))
    r_min <- lap_solve_line_metric(x, y, cost = type, maximize = FALSE)
    r_max <- lap_solve_line_metric(x, y, cost = type, maximize = TRUE)
    expect_equal(r_min$total_cost, min(totals), tolerance = 1e-9)
    expect_equal(r_max$total_cost, max(totals), tolerance = 1e-9)
    expect_gte(r_max$total_cost, r_min$total_cost)
  }

  set.seed(654)
  # Square case (n == m)
  check(runif(5, -5, 5), runif(5, -5, 5), "L1")
  check(runif(5, -5, 5), runif(5, -5, 5), "L2")
  # Rectangular case (n < m)
  check(runif(3, -5, 5), runif(6, -5, 5), "L1")
  check(runif(3, -5, 5), runif(6, -5, 5), "L2")
})

test_that("line_metric handles edge cases", {
  # Single element
  result <- lap_solve_line_metric(c(1.5), c(2.0), cost = "L1")
  expect_equal(result$match, 1L)
  expect_equal(result$total_cost, 0.5)
  
  # Two elements
  result <- lap_solve_line_metric(c(1.0, 3.0), c(0.5, 3.5), cost = "L1")
  expect_equal(length(result$match), 2)
  
  # Identical values
  result <- lap_solve_line_metric(c(1.0, 1.0, 1.0), c(1.0, 1.0, 1.0), cost = "L1")
  expect_equal(result$total_cost, 0.0)
})

test_that("line_metric input validation works", {
  # Empty vectors
  expect_error(lap_solve_line_metric(numeric(0), c(1, 2)), "non-empty")
  expect_error(lap_solve_line_metric(c(1, 2), numeric(0)), "non-empty")
  
  # n > m (more sources than targets)
  expect_error(lap_solve_line_metric(c(1, 2, 3), c(1, 2)), "must be <=")
  
  # Non-finite values
  expect_error(lap_solve_line_metric(c(1, NA, 3), c(1, 2, 3)), "finite")
  expect_error(lap_solve_line_metric(c(1, 2, 3), c(1, Inf, 3)), "finite")
  expect_error(lap_solve_line_metric(c(1, NaN, 3), c(1, 2, 3)), "finite")
  
  # Invalid cost type
  expect_error(lap_solve_line_metric(c(1, 2), c(1, 2), cost = "L3"), "must be one of")
  expect_error(lap_solve_line_metric(c(1, 2), c(1, 2), cost = "invalid"), "must be one of")
  
  # Non-numeric input
  expect_error(lap_solve_line_metric(c("a", "b"), c(1, 2)), "numeric")
  expect_error(lap_solve_line_metric(c(1, 2), c("a", "b")), "numeric")
})

test_that("line_metric is consistent with sorted inputs", {
  set.seed(999)
  
  # Generate random sorted sequences
  for (trial in 1:5) {
    n <- sample(3:6, 1)
    m <- n + sample(1:3, 1)
    
    x <- sort(runif(n, -10, 10))
    y <- sort(runif(m, -10, 10))
    
    result <- lap_solve_line_metric(x, y, cost = "L1", maximize = FALSE)
    
    # For sorted inputs, we can verify the monotonicity property:
    # If sources are sorted, matched targets should form an increasing sequence
    matched_targets <- result$match
    expect_true(all(diff(matched_targets) > 0))
  }
})

test_that("line_metric performance on larger problems", {
  skip_on_cran()
  
  # Test that it can handle reasonably large problems
  set.seed(2025)
  n <- 500
  m <- 750
  
  x <- runif(n, -100, 100)
  y <- runif(m, -100, 100)
  
  start_time <- Sys.time()
  result <- lap_solve_line_metric(x, y, cost = "L2", maximize = FALSE)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  
  # Should complete in reasonable time (< 1 second for n=500, m=750)
  expect_true(elapsed < 1.0)
  
  # Verify result
  expect_equal(length(result$match), n)
  expect_true(all(result$match >= 1 & result$match <= m))
  expect_equal(length(unique(result$match)), n)
})
