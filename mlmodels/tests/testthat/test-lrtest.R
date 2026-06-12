# tests/testthat/test-lrtest.R
library(testthat)

# Basic functionality
test_that("lrtest works with nested homoskedastic models", {
  data(mtcars)

  fit_restricted <- ml_lm(mpg ~ wt + hp, data = mtcars)
  fit_full       <- ml_lm(mpg ~ wt + hp + qsec + drat, data = mtcars)

  lr <- lrtest(fit_restricted, fit_full)

  expect_s3_class(lr, "lrtest.mlmodel")
  expect_true(is.numeric(lr$chisq))
  expect_true(is.numeric(lr$pval))
  expect_true(lr$df > 0)
})

test_that("lrtest works with nested heteroskedastic models", {
  data(mtcars)

  fit_restricted <- ml_lm(mpg ~ wt + hp, scale = ~ wt, data = mtcars)
  fit_full       <- ml_lm(mpg ~ wt + hp + qsec, scale = ~ wt + hp, data = mtcars)

  lr <- lrtest(fit_restricted, fit_full)

  expect_s3_class(lr, "lrtest.mlmodel")
  expect_true(lr$df > 0)
})

test_that("lrtest correctly identifies restricted vs unrestricted model", {
  data(mtcars)

  fit_small <- ml_lm(mpg ~ wt, data = mtcars)
  fit_large <- ml_lm(mpg ~ wt + hp + qsec, data = mtcars)

  lr <- lrtest(fit_small, fit_large)

  expect_equal(lr$npar_r, 3)
  expect_equal(lr$npar_f, 5)
  expect_equal(lr$df, 2)
})

test_that("lrtest fails when models have the same number of parameters", {
  data(mtcars)

  fit1 <- ml_lm(mpg ~ wt + hp, data = mtcars)
  fit2 <- ml_lm(mpg ~ wt + hp, data = mtcars)   # same complexity

  expect_error(
    lrtest(fit1, fit2),
    "same number of parameters"
  )
})

test_that("lrtest fails when models are from different classes", {
  data(mtcars)
  fit_lm <- ml_lm(mpg ~ wt + hp, data = mtcars)

  expect_error(
    lrtest(fit_lm, mtcars),
    "must inherit from class 'mlmodel'"
  )
})

test_that("print.lrtest.mlmodel produces readable output", {
  data(mtcars)
  fit_restricted <- ml_lm(mpg ~ wt, data = mtcars)
  fit_full       <- ml_lm(mpg ~ wt + hp, data = mtcars)

  lr <- lrtest(fit_restricted, fit_full)

  expect_output(print(lr), "Likelihood Ratio Test")
  expect_output(print(lr), "Chisq")
  expect_output(print(lr), "LogLik")
})
