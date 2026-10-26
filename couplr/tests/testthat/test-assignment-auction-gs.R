# Test file for Gauss-Seidel Auction Algorithm
# Save as: tests/testthat/test-assignment-auction-gs.R

test_that("auction_gs works on small square matrix", {
  set.seed(42)
  n <- 5
  M <- matrix(runif(n*n), n, n)
  
  # Compare with other methods
  a_gs <- assignment(M, method = "auction_gs")
  a_jv <- assignment(M, method = "jv")
  
  expect_true(is.list(a_gs))
  expect_true("match" %in% names(a_gs))
  expect_true("total_cost" %in% names(a_gs))
  expect_equal(length(a_gs$match), n)
  
  # Should get same (or very close) cost as JV
  expect_equal(a_gs$total_cost, a_jv$total_cost, tolerance = 1e-6)
})

test_that("auction_gs handles rectangular n < m", {
  set.seed(43)
  n <- 4; m <- 7
  M <- matrix(runif(n*m), n, m)
  
  a_gs <- assignment(M, method = "auction_gs")
  a_ssp <- assignment(M, method = "ssp")
  
  expect_equal(length(a_gs$match), n)
  expect_true(all(a_gs$match >= 1 & a_gs$match <= m))
  expect_equal(a_gs$total_cost, a_ssp$total_cost, tolerance = 1e-6)
})

test_that("auction_gs handles rectangular n > m (auto-transpose)", {
  set.seed(44)
  n <- 8; m <- 4
  M <- matrix(runif(n*m), n, m)
  
  # This should work with auto-transpose
  a_gs <- assignment(M, method = "auction_gs")
  a_ssp <- assignment(M, method = "ssp")
  
  expect_equal(length(a_gs$match), n)
  # When n > m, only m rows can be matched, rest will be 0 (unmatched)
  matched <- a_gs$match[a_gs$match > 0]
  expect_true(all(matched >= 1 & matched <= m))
  expect_equal(sum(a_gs$match > 0), m)  # Exactly m assignments
  expect_equal(a_gs$total_cost, a_ssp$total_cost, tolerance = 1e-6)
})

test_that("auction_gs handles NA values", {
  set.seed(45)
  n <- 5; m <- 7
  M <- matrix(runif(n*m), n, m)
  
  # Set some entries to NA (forbidden)
  M[1, 1:3] <- NA
  M[2, c(2,4,6)] <- NA
  M[5, 1] <- NA
  
  a_gs <- assignment(M, method = "auction_gs")
  a_ssp <- assignment(M, method = "ssp")
  
  # Check that no NA cells were chosen
  for (i in 1:n) {
    j <- a_gs$match[i]
    expect_false(is.na(M[i, j]))
  }
  
  expect_equal(a_gs$total_cost, a_ssp$total_cost, tolerance = 1e-6)
})

test_that("auction_gs handles Inf values", {
  set.seed(46)
  n <- 4; m <- 6
  M <- matrix(runif(n*m), n, m)
  
  # Set some entries to Inf (forbidden)
  M[1, c(1,3,5)] <- Inf
  M[3, 2:4] <- Inf
  
  a_gs <- assignment(M, method = "auction_gs")
  
  # Check that no Inf cells were chosen
  for (i in 1:n) {
    j <- a_gs$match[i]
    expect_true(is.finite(M[i, j]))
  }
})

test_that("auction_gs errors when row has no valid assignments", {
  M <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    NA, NA, NA
  ), nrow = 3, byrow = TRUE)
  
  expect_error(assignment(M, method = "auction_gs"), "Infeasible")
})

test_that("auction_gs maximization works", {
  set.seed(47)
  n <- 5
  M <- matrix(runif(n*n), n, n)
  
  a_max <- assignment(M, maximize = TRUE, method = "auction_gs")
  a_min <- assignment(-M, maximize = FALSE, method = "auction_gs")
  
  expect_equal(a_max$total_cost, -a_min$total_cost, tolerance = 1e-6)
})

test_that("auction_gs with custom epsilon", {
  set.seed(48)
  n <- 4
  M <- matrix(runif(n*n), n, n)
  
  # Try different epsilon values
  a1 <- assignment(M, method = "auction_gs", auction_eps = 1e-6)
  a2 <- assignment(M, method = "auction_gs", auction_eps = 1e-9)
  a3 <- assignment(M, method = "auction_gs", auction_eps = 1e-12)
  
  # All should give valid assignments
  expect_true(all(a1$match >= 1 & a1$match <= n))
  expect_true(all(a2$match >= 1 & a2$match <= n))
  expect_true(all(a3$match >= 1 & a3$match <= n))
  
  # Smaller epsilon should give more accurate results
  expect_lte(abs(a3$total_cost - a2$total_cost), abs(a2$total_cost - a1$total_cost))
})

test_that("auction_gs returns bid count diagnostic", {
  set.seed(49)
  n <- 6
  M <- matrix(runif(n*n), n, n)
  
  result <- lap_solve_auction_gs(M, maximize = FALSE, eps = 1e-6)
  
  expect_true("bids" %in% names(result))
  expect_true(is.numeric(result$bids))
  expect_true(result$bids > 0)
  expect_true(result$bids <= 200000000)  # Should be well below iteration guard
})

test_that("auction_gs compares favorably with standard auction", {
  set.seed(50)
  n <- 10
  M <- matrix(runif(n*n), n, n)
  
  result_gs <- lap_solve_auction_gs(M, maximize = FALSE, eps = 1e-8)
  result_std <- lap_solve_auction(M, maximize = FALSE, eps = 1e-8)
  
  # Should get same cost (within tolerance)
  expect_equal(result_gs$total_cost, result_std$total_cost, tolerance = 1e-6)
  
  # GS often makes fewer bids (though not guaranteed)
  # Just check that both terminated successfully
  expect_true("bids" %in% names(result_gs))
})

test_that("auction_gs works on larger problems", {
  testthat::skip_on_cran()
  
  set.seed(51)
  n <- 100; m <- 150
  M <- matrix(runif(n*m), n, m)
  
  # Add some NA entries
  na_idx <- sample.int(length(M), size = as.integer(0.1 * length(M)))
  M[na_idx] <- NA
  
  # Ensure each row has at least one valid entry
  for (i in 1:n) {
    if (all(is.na(M[i,]))) {
      j <- sample.int(m, 1)
      M[i, j] <- runif(1)
    }
  }
  
  a_gs <- assignment(M, method = "auction_gs")
  
  expect_equal(length(a_gs$match), n)
  expect_true(all(a_gs$match >= 1 & a_gs$match <= m))
  expect_true(all(!is.na(M[cbind(1:n, a_gs$match)])))
})
