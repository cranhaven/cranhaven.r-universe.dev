# Tests for classes exposed in src/Module-utils.cpp
# Classes are tested in the order they appear in the C++ file

# 1. CppObject ----
test_that("CppObject base class", {
  skip("Base class - tested via derived classes")
})

# 2. CppMap ----
test_that("CppMap class", {
  skip("Tested via SystemHistory properties (UnitHeads, PatientHeads, etc.)")
})

# RRandom (defined in RRandom.h, exposed via Module-utils) ----
test_that("RRandom runif method", {
  rr <- RRandom$new()

  runif(1)
  seed <- .GlobalEnv$.Random.seed

  a <- rr$runif()
  b <- rr$runif()
  .GlobalEnv$.Random.seed <- seed
  c <- rr$runif()
  .GlobalEnv$.Random.seed <- seed
  d <- runif(1)

  expect_true(0 <= a && a <= 1)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)
})

test_that("RRandom runif2 method", {
  rr <- RRandom$new()

  seed <- .GlobalEnv$.Random.seed
  a <- rr$runif2(50, 100)
  b <- rr$runif2(50, 100)
  .GlobalEnv$.Random.seed <- seed
  c <- rr$runif2(50, 100)
  .GlobalEnv$.Random.seed <- seed
  d <- runif(1, 50, 100)

  expect_true(50 <= a && a <= 100)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)
})

test_that("RRandom rexp method", {
  rr <- RRandom$new()

  seed <- .GlobalEnv$.Random.seed
  a <- rr$rexp()
  b <- rr$rexp()
  .GlobalEnv$.Random.seed <- seed
  c <- rr$rexp()
  .GlobalEnv$.Random.seed <- seed
  d <- rexp(1)

  expect_true(0 <= a)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)
})

test_that("RRandom rexp1 method", {
  rr <- RRandom$new()

  seed <- .GlobalEnv$.Random.seed
  a <- rr$rexp1(100)
  b <- rr$rexp1(100)
  .GlobalEnv$.Random.seed <- seed
  c <- rr$rexp1(100)
  .GlobalEnv$.Random.seed <- seed
  d <- rexp(1, 100)

  expect_true(0 <= a)
  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)
})

test_that("RRandom rgamma method", {
  rr <- RRandom$new()

  seed <- .GlobalEnv$.Random.seed
  a <- rr$rgamma(1, 1)
  b <- rr$rgamma(1, 1)
  .GlobalEnv$.Random.seed <- seed
  c <- rr$rgamma(1, 1)
  .GlobalEnv$.Random.seed <- seed
  d <- rgamma(1, 1)

  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)
})

test_that("RRandom rnorm method", {
  rr <- RRandom$new()

  seed <- .GlobalEnv$.Random.seed
  a <- rr$rnorm()
  b <- rr$rnorm()
  .GlobalEnv$.Random.seed <- seed
  c <- rr$rnorm()
  .GlobalEnv$.Random.seed <- seed
  d <- rnorm(1)

  expect_false(a == b)
  expect_true(a == c)
  expect_true(a == d)
})

test_that("RRandom rpoisson method", {
  rr <- RRandom$new()

  seed <- .GlobalEnv$.Random.seed
  a <- rr$rpoisson(5)
  b <- rr$rpoisson(5)
  .GlobalEnv$.Random.seed <- seed
  c <- rr$rpoisson(5)
  .GlobalEnv$.Random.seed <- seed
  d <- rpois(1, 5)

  # Verify seed restoration works
  expect_identical(a, c, info = "Restoring seed should give identical result")
  expect_identical(a, as.numeric(d), info = "RRandom should match R's rpois exactly (after conversion to double)")
  
  # Verify seed advances (draw 10 values and check they're not all identical)
  .GlobalEnv$.Random.seed <- seed
  values <- replicate(10, rr$rpoisson(5))
  expect_true(length(unique(values)) > 1, 
              info = "Seed should advance: 10 draws should not all be identical")
})
