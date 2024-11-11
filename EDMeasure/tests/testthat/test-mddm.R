context("mddm")

# setup
num_obs <- 10
num_comp <- 2

test_that("mddm C vs. mddm R univariate X univariate Y", {
  X <- rnorm(num_obs)
  Y <- rnorm(num_obs)

  m1 <- EDMeasure::mddm(X, Y, compute = "C")
  m2 <- EDMeasure::mddm(X, Y, compute = "R")

  expect_equal(m1, m2)
})

test_that("mddm C vs. mddm R univariate X multivariate Y", {
  X <- rnorm(num_obs)
  Y <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)

  m1 <- EDMeasure::mddm(X, Y, compute = "C")
  m2 <- EDMeasure::mddm(X, Y, compute = "R")

  expect_equal(m1, m2)
})

test_that("mddm C vs. mddm R multivariate X univariate Y", {
  X <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)
  Y <- rnorm(num_obs)

  m1 <- EDMeasure::mddm(X, Y, compute = "C")
  m2 <- EDMeasure::mddm(X, Y, compute = "R")

  expect_equal(m1, m2)
})

test_that("mddm C vs. mddm R multivariate X multivariate Y", {
  X <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)
  Y <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)

  m1 <- EDMeasure::mddm(X, Y, compute = "C")
  m2 <- EDMeasure::mddm(X, Y, compute = "R")

  expect_equal(m1, m2)
})

test_that("mdd vs. mddm univariate X univariate Y", {
  X <- rnorm(num_obs)
  Y <- rnorm(num_obs)

  m1 <- EDMeasure::mdd(X, Y, center = "D")
  m2 <- EDMeasure::mddm(X, Y)

  expect_equal(m1, as.numeric(m2))
})

test_that("mdd vs. mddm univariate X multivariate Y", {
  X <- rnorm(num_obs)
  Y <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)

  m1 <- EDMeasure::mdd(X, Y[, 1], center = "D")
  m2 <- EDMeasure::mdd(X, Y[, 2], center = "D")
  m3 <- EDMeasure::mdd(X, Y, center = "D")
  m4 <- EDMeasure::mddm(X, Y)

  expect_equal(c(m1, m2), diag(m4))
  expect_equal(m3, sum(diag(m4)))
})

test_that("mdd vs. mddm multivariate X univariate Y", {
  X <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)
  Y <- rnorm(num_obs)

  m1 <- EDMeasure::mdd(X, Y, center = "D")
  m2 <- EDMeasure::mddm(X, Y)

  expect_equal(m1, as.numeric(m2))
})

test_that("mdd vs. mddm multivariate X multivariate Y", {
  X <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)
  Y <- matrix(rnorm(num_obs * num_comp), num_obs, num_comp)

  m1 <- EDMeasure::mdd(X, Y[, 1], center = "D")
  m2 <- EDMeasure::mdd(X, Y[, 2], center = "D")
  m3 <- EDMeasure::mdd(X, Y, center = "D")
  m4 <- EDMeasure::mddm(X, Y)

  expect_equal(c(m1, m2), diag(m4))
  expect_equal(m3, sum(diag(m4)))
})

