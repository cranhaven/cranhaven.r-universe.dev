context("is.formula")

test_that("'is.formula' does the job", {
  expect_true(!is.formula(4) &&
                is.formula(~ x + y) && 
                is.formula(z ~ x + y))
})
