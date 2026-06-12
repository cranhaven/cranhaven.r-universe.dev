# tests/testthat/test-vcov.R
library(testthat)

test_that("vcov.mlmodel returns matrix with correct dimensions", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp + qsec, data = mtcars)

  v <- vcov(fit, type = "oim")

  expect_true(is.matrix(v))
  expect_equal(nrow(v), 5)  # intercept + wt + hp + qsec
  expect_equal(ncol(v), 5)
  expect_equal(rownames(v), colnames(v))
})

test_that("vcov.mlmodel supports different types", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, data = mtcars)

  expect_silent(vcov(fit, type = "oim"))
  expect_silent(vcov(fit, type = "robust"))
  expect_error(vcov(fit, type = "cluster"), "cl_var")
})
