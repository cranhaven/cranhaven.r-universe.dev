context("almost.zero")

test_that("'almost.zero' works", {
  expect_identical(almost.zero(c(0, 10^(-7), 10^(-8))), 
                   c(TRUE, FALSE, TRUE))
})
