test_that("csflow == ssp on small squares", {
  set.seed(1)
  for (n in 2:7) {
    M <- matrix(runif(n*n), n)
    a <- assignment(M, method = "ssp")
    c <- assignment(M, method = "csflow")
    expect_equal(a$total_cost, c$total_cost, tolerance = 1e-10)
  }
})

test_that("csflow handles rectangles and NA", {
  set.seed(2)
  n <- 5; m <- 9
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = 8)] <- NA
  a <- assignment(M, method = "ssp")
  c <- assignment(M, method = "csflow")
  expect_equal(a$total_cost, c$total_cost, tolerance = 1e-10)
})

test_that("csflow works when m < n (auto-transpose)", {
  set.seed(3)
  n <- 9; m <- 5
  M <- matrix(runif(n*m), n, m)
  a <- assignment(M, method = "ssp")
  c <- assignment(M, method = "csflow")
  expect_equal(a$total_cost, c$total_cost, tolerance = 1e-10)
})

test_that("csflow errors when a row is entirely forbidden", {
  M <- matrix(1, 3, 5)
  M[1,] <- NA
  expect_error(assignment(M, method = "csflow"), "Infeasible")
})

test_that("maximize works for csflow", {
  set.seed(4)
  n <- 6
  M <- matrix(runif(n*n), n)
  a <- assignment(M, maximize = TRUE,  method = "csflow")
  b <- assignment(-M, maximize = FALSE, method = "csflow")
  expect_equal(a$total_cost, -b$total_cost, tolerance = 1e-10)
})

test_that("csflow matches ssp on larger sparse rectangular", {
  testthat::skip_on_cran()
  set.seed(5)
  n <- 80; m <- 150
  M <- matrix(Inf, n, m)
  # ~5% density with finite costs
  nz <- ceiling(0.05 * length(M))
  idx <- sample.int(length(M), nz)
  M[idx] <- runif(nz)
  # ensure feasibility: give each row at least one finite entry
  for (i in 1:n) {
    if (all(!is.finite(M[i,]))) {
      j <- sample.int(m, 1)
      M[i, j] <- runif(1)
    }
  }
  a <- assignment(M, method = "ssp")
  c <- assignment(M, method = "csflow")
  expect_equal(a$total_cost, c$total_cost, tolerance = 1e-8)
})
