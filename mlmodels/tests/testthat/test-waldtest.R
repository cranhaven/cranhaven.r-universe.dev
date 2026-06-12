# tests/testthat/test-waldtest.R
library(testthat)

test_that("waldtest works with indices", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp + qsec, data = mtcars)

  w <- waldtest(fit, indices = c(2, 3))  # test wt and hp

  expect_s3_class(w, "waldtest.mlmodel")
  expect_true(is.numeric(w$waldstat))
  expect_true(is.numeric(w$pval))
  expect_equal(w$df, 2)
})

test_that("waldtest works with coef_names", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp + qsec, data = mtcars)

  w <- waldtest(fit, coef_names = c("value::wt", "value::hp"))

  expect_s3_class(w, "waldtest.mlmodel")
  expect_equal(w$df, 2)
})

test_that("waldtest works with rest_matrix", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp + qsec, data = mtcars)

  # Test hypothesis: wt + hp = 0
  R <- matrix(c(0, 1, 1, 0, 0), nrow = 1, byrow = TRUE)  # β_wt + β_hp = 0
  w <- waldtest(fit, rest_matrix = R, rhs = 0)

  expect_s3_class(w, "waldtest.mlmodel")
  expect_equal(w$df, 1)
})

test_that("waldtest handles singular matrix gracefully", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, data = mtcars)

  # This may trigger singularity depending on data
  expect_silent({
    w <- waldtest(fit, indices = 1:2, vcov.type = "oim")
  })
})

test_that("print.waldtest.mlmodel works without error", {
  data(mtcars)
  fit <- ml_lm(mpg ~ wt + hp, data = mtcars)
  w <- waldtest(fit, indices = 2)

  expect_output(print(w), "Wald Test of Linear Restrictions")
  expect_output(print(w), "Chisq")
})
