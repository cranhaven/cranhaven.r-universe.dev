context("mdd")

# setup
num_obs <- 10
num_comp <- 2

test_that("mdd C vs. mdd R univariate X univariate Y", {
  X <- rnorm(num_obs)
  Y <- rnorm(num_obs)

  m1 <- EDMeasure::mdd(X, Y, compute = "C", center = "U")
  m2 <- EDMeasure::mdd(X, Y, compute = "R", center = "U")

  expect_equal(m1, m2)

  m3 <- EDMeasure::mdd(X, Y, compute = "C", center = "D")
  m4 <- EDMeasure::mdd(X, Y, compute = "R", center = "D")

  expect_equal(m3, m4)
})

test_that("mdd C vs. mdd R univariate X multivariate Y", {
  X <- rnorm(num_obs)
  Y <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)

  m1 <- EDMeasure::mdd(X, Y, compute = "C", center = "U")
  m2 <- EDMeasure::mdd(X, Y, compute = "R", center = "U")

  expect_equal(m1, m2)

  m3 <- EDMeasure::mdd(X, Y, compute = "C", center = "D")
  m4 <- EDMeasure::mdd(X, Y, compute = "R", center = "D")

  expect_equal(m3, m4)
})

test_that("mdd C vs. mdd R multivariate X univariate Y", {
  X <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)
  Y <- rnorm(num_obs)

  m1 <- EDMeasure::mdd(X, Y, compute = "C", center = "U")
  m2 <- EDMeasure::mdd(X, Y, compute = "R", center = "U")

  expect_equal(m1, m2)

  m3 <- EDMeasure::mdd(X, Y, compute = "C", center = "D")
  m4 <- EDMeasure::mdd(X, Y, compute = "R", center = "D")

  expect_equal(m3, m4)
})

test_that("mdd C vs. mdd R multivariate X multivariate Y", {
  X <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)
  Y <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)

  m1 <- EDMeasure::mdd(X, Y, compute = "C", center = "U")
  m2 <- EDMeasure::mdd(X, Y, compute = "R", center = "U")

  expect_equal(m1, m2)

  m3 <- EDMeasure::mdd(X, Y, compute = "C", center = "D")
  m4 <- EDMeasure::mdd(X, Y, compute = "R", center = "D")

  expect_equal(m3, m4)
})

