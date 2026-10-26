test_that("cycle_cancel solves small problems correctly", {
  set.seed(123)

  for (n in 3:5) {
    cost <- matrix(runif(n*n, 0, 50), nrow = n)

    result <- assignment(cost, maximize = FALSE, method = "cycle_cancel")
    
    expect_equal(length(result$match), n)
    expect_true(all(result$match >= 1 & result$match <= n))
    expect_equal(length(unique(result$match)), n)
    
    manual_cost <- sum(sapply(1:n, function(i) cost[i, result$match[i]]))
    expect_equal(result$total_cost, manual_cost, tolerance = 1e-10)
  }
})

test_that("cycle_cancel handles rectangular problems", {
  set.seed(456)

  n <- 4
  m <- 6
  cost <- matrix(runif(n*m, 0, 50), nrow = n, ncol = m)

  result <- assignment(cost, maximize = FALSE, method = "cycle_cancel")
  
  expect_equal(length(result$match), n)
  expect_true(all(result$match >= 1 & result$match <= m))
  expect_equal(length(unique(result$match)), n)
  
  manual_cost <- sum(sapply(1:n, function(i) cost[i, result$match[i]]))
  expect_equal(result$total_cost, manual_cost, tolerance = 1e-10)
})

test_that("cycle_cancel handles maximize parameter", {
  set.seed(789)
  n <- 4
  cost <- matrix(runif(n*n, 0, 50), nrow = n)

  r_min <- assignment(cost, maximize = FALSE, method = "cycle_cancel")
  r_max <- assignment(cost, maximize = TRUE, method = "cycle_cancel")
  
  cost_min <- sum(sapply(1:n, function(i) cost[i, r_min$match[i]]))
  cost_max <- sum(sapply(1:n, function(i) cost[i, r_max$match[i]]))
  
  expect_true(cost_max >= cost_min)
  expect_equal(r_max$total_cost, cost_max, tolerance = 1e-10)
})

test_that("cycle_cancel agrees with other solvers", {
  set.seed(2025)

  for (trial in 1:3) {
    n <- sample(3:5, 1)
    cost <- matrix(runif(n*n, 0, 50), nrow = n)

    result_cc <- assignment(cost, maximize = FALSE, method = "cycle_cancel")
    result_jv <- assignment(cost, method = "jv", maximize = FALSE)
    result_hungarian <- assignment(cost, method = "hungarian", maximize = FALSE)
    
    expect_equal(result_cc$total_cost, result_jv$total_cost, tolerance = 1e-9)
    expect_equal(result_cc$total_cost, result_hungarian$total_cost, tolerance = 1e-9)
  }
})

test_that("cycle_cancel handles NA/forbidden edges", {
  set.seed(101)
  n <- 4
  m <- 5
  cost <- matrix(runif(n*m, 0, 50), nrow = n, ncol = m)

  cost[1, 1] <- NA
  cost[2, 2] <- NA

  result <- assignment(cost, method = "cycle_cancel")
  
  expect_equal(length(result$match), n)
  expect_true(all(result$match >= 1 & result$match <= m))
  
  for (i in 1:n) {
    expect_false(is.na(cost[i, result$match[i]]))
  }
})

test_that("cycle_cancel handles edge cases", {
  cost <- matrix(5, nrow = 1, ncol = 1)
  result <- assignment(cost, method = "cycle_cancel")
  expect_equal(result$match, 1L)
  expect_equal(result$total_cost, 5)

  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- assignment(cost, method = "cycle_cancel")
  expect_equal(length(result$match), 2)
  expect_equal(result$total_cost, 5, tolerance = 1e-10)
})

test_that("cycle_cancel input validation works", {
  expect_error(assignment(matrix(numeric(0), nrow = 0, ncol = 0), method = "cycle_cancel"), "at least one")

  cost <- matrix(NA_real_, nrow = 3, ncol = 3)
  expect_error(assignment(cost, method = "cycle_cancel"), "Infeasible")
})

test_that("cycle_cancel method_used attribute is set", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$method_used, "cycle_cancel")
  expect_true(inherits(result, "lap_solve_result"))
})

test_that("cycle_cancel performance on modest sizes", {
  skip_on_cran()

  set.seed(2025)

  for (n in c(10, 20)) {
    cost <- matrix(runif(n*n, 0, 50), nrow = n)

    start_time <- Sys.time()
    result <- assignment(cost, method = "cycle_cancel")
    elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
    
    expect_true(elapsed < 5.0)
    
    expect_equal(length(result$match), n)
    manual_cost <- sum(sapply(1:n, function(i) cost[i, result$match[i]]))
    expect_equal(result$total_cost, manual_cost, tolerance = 1e-10)
  }
})
