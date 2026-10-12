context("gclm optimization")

test_that("clyap obtain the solution",{
  ## generate random stable matrix
  B <- matrix(nrow = 10, ncol = 10, rnorm(100))
  diag(B) <- apply(B, MARGIN = 1, function(x) -sum(abs(x)))
  ## random diagonal matrix
  C <- diag(runif(10))
  ## 
  X <- clyap(B, C)
  expect_equal(max(abs(B %*% X + X %*% t(B) + C)), 0)
})


test_that("clyap ",{
  ## generate random stable matrix
  B <- matrix(nrow = 10, ncol = 10, rnorm(100))
  diag(B) <- apply(B, MARGIN = 1, function(x) -sum(abs(x)))
  ## random diagonal matrix
  C <- diag(runif(10))
  ## 
  all <- clyap(B, C, all = TRUE)
  expect_equal(max(abs(B %*% all$X + all$X %*% t(B) + C)), 0)
  
  C <- diag(runif(10))
  X <- clyap(all$B, C = C, Q = all$Q)
  expect_equal(max(abs(B %*% X + X %*% t(B) + C)), 0)
})

test_that("Optimization fast if initialized in optimum (loglik)", {
  ## generate random stable matrix
  B <- matrix(nrow = 10, ncol = 10, rnorm(100))
  diag(B) <- apply(B, MARGIN = 1, function(x) -sum(abs(x)))
  Sigma <- clyap(B, diag(10))
  res <- gclm(Sigma, B, eps = 0, lambda = 0, lambdac = -1)
  expect_equal(res$B, B)
  expect_equal(res$diff, 0)
  expect_lt(res$iter, 10)
})


test_that("Optimization fast if initialized in optimum (frobenius)", {
  ## generate random stable matrix
  B <- matrix(nrow = 10, ncol = 10, rnorm(100))
  diag(B) <- apply(B, MARGIN = 1, function(x) -sum(abs(x)))
  Sigma <- clyap(B, diag(10))
  res <- gclm(Sigma, B, eps = 1e-16, lambda = 0, lambdac = -1, 
              loss = "frobenius")
  expect_equal(res$B, B)
  expect_equal(res$diff, 0)
  expect_lt(res$iter, 10)
})
