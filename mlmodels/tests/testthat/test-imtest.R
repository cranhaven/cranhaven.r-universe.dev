# tests/testthat/test-imtest.R

library(testthat)

test_that("IMtest runs with default method (opg)", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp + qsec, data = mtcars)

  im <- IMtest(fit, method = "opg")

  expect_s3_class(im, "IMtest.mlmodel")
  expect_true(is.numeric(im$tstat))
  expect_true(is.numeric(im$pval$analytical))
  expect_equal(im$version$method, "opg")
})

test_that("IMtest works with quadratic method", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, data = mtcars)

  im <- IMtest(fit, method = "quad")

  expect_s3_class(im, "IMtest.mlmodel")
  expect_equal(im$version$method, "quad")
})

test_that("IMtest works with bootstrap methods", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, data = mtcars)

  im_boot <- IMtest(fit, method = "boot_opg", repetitions = 5)  # small R for speed

  expect_s3_class(im_boot, "IMtest.mlmodel")
  expect_true(is.list(im_boot$pval))
  expect_true("analytical" %in% names(im_boot$pval))
  expect_true("bootstrapped" %in% names(im_boot$pval))
})

test_that("print.IMtest produces output without error", {
  library(wooldridge)
  data("smoke")
  
  fit <- ml_lm(cigs ~ cigpric + income + age,
                  data = smoke)

  im <- IMtest(fit, method = "opg")

  expect_output(print(im), "Information Matrix Test")
  expect_output(print(im), "Chisq")
})

test_that("IMtest fails gracefully with missing components", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, data = mtcars)

  # Remove gradientObs to simulate bad object
  fit$gradientObs <- NULL

  expect_error(IMtest(fit), "gradientObs")
})
