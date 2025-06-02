context("nin")

test_that("'nin' is the negation of 'in'", {
  x <- 1:10 %nin% c(1,3,5,9)
  y <- 1:10 %in% c(1,3,5,9)
  expect_identical(x, !y)
})
