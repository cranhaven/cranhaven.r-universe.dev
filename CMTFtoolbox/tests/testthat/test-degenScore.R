test_that("the function works normally", {
  S = matrix(rnorm(100), nrow = 10, ncol = 10)
  expect_no_error(degenScore(S))
})
