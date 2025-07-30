test_that("fit_bart_model with wrong response variable y", {
  expect_error(fit_bart_model(y = c(0, 1, 2), x = data.frame()), "must be a numeric vector")
})

test_that("fit_bart_model with wrongly formatted covariates x", {
  expect_error(fit_bart_model(y = c(0, 0, 1), x = matrix(1:9, nrow = 3)), "must be a data frame")
})

test_that("fit_bart_model with different length of y and x", {
  expect_error(fit_bart_model(y = c(0, 0), x = data.frame(x1 = c(1, 2, 3))), "must match the number of rows")
})

