test_that('testing collapse with 100x100x30x10; keep=c(4,2); sum', {
  dim <- c(100, 10, 30, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4, 2)

  aggregate <- sum
  avg <- 0L
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x0x10; keep=c(4,2); sum', {
  dim <- c(100, 10, 0, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4, 2)

  aggregate <- sum
  avg <- 0L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x30x10; keep=4; sum', {
  dim <- c(100, 10, 0, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- 4

  aggregate <- sum
  avg <- 0L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x30x10; keep=c(4,3,2,1); sum', {
  dim <- c(100, 10, 30, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4,3,2,1)

  aggregate <- sum
  avg <- 0L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x0x10; keep=c(4,3,2,1); sum', {
  dim <- c(100, 10, 0, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4,3,2,1)

  aggregate <- sum
  avg <- 0L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})



test_that('testing collapse with 100x100x30x10; keep=c(4,2); mean', {
  dim <- c(100, 10, 30, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4, 2)

  aggregate <- mean
  avg <- 1L
  method <- "asis"
  r1 <- r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x0x10; keep=c(4,2); mean', {
  dim <- c(100, 10, 0, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4, 2)

  aggregate <- mean
  avg <- 1L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x30x10; keep=4; mean', {
  dim <- c(100, 10, 0, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- 4

  aggregate <- mean
  avg <- 1L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x30x10; keep=c(4,3,2,1); mean', {
  dim <- c(100, 10, 30, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4,3,2,1)

  aggregate <- mean
  avg <- 1L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})

test_that('testing collapse with 100x100x0x10; keep=c(4,3,2,1); mean', {
  dim <- c(100, 10, 0, 10)
  x <- array(rnorm(prod(dim)), dim = dim)

  keep <- c(4,3,2,1)

  aggregate <- mean
  avg <- 1L

  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  x <- abs(x)
  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(10.0 * log10(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(x * x) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(sqrt(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)


  x <- array(rnorm(prod(dim)), dim = dim) * 1i + array(rnorm(prod(dim)), dim = dim)
  method <- "asis"
  r1 <- apply(x, keep, aggregate)
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "10log10"
  r1 <- apply(x, keep, function(x){ aggregate(20.0 * log10(Mod(x))) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "square"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)^2) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

  method <- "sqrt"
  r1 <- apply(x, keep, function(x){ aggregate(Mod(x)) })
  r2 <- ravetools::collapse(x, keep, avg, method)
  testthat::expect_equal(r1, r2)

})



