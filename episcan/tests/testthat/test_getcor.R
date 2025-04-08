library(episcan)

context("test getcor")

test_that('pearson correlation',{
  a <- matrix(rnorm(40), nrow = 10)
  b <- matrix(rnorm(60), nrow = 10)
  expect_equal(getcor(a, b),
               cor(a, b))
})