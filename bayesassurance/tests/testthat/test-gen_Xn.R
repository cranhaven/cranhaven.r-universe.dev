library(testthat)
library(ggplot2)
library(bayesassurance)


# gen_Xn() checks
test_that("Correct number of rows and columns", {
  n <- c(1,3,5,8)
  out <- gen_Xn(n = n)
  
  expect_equal(nrow(out), 17)
  expect_equal(ncol(out), 4)
})



test_that("Correct entries in columns", {
  n <- c(1,3,5,8)
  out <- gen_Xn(n = n)
  
  expect_equal(out[,1], c(1, rep(0, 16)))
  expect_equal(out[,2], c(0, rep(1, 3), rep(0, 13)))
  expect_equal(out[,3], c(rep(0, 4), rep(1, 5), rep(0, 8)))
  expect_equal(out[,4], c(rep(0, 9), rep(1, 8)))
})

