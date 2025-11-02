library(testthat)
library(ggplot2)
library(bayesassurance)


# genXn_longitudinal() checks
test_that("Correct number of rows and columns", {
  ids <- c(1,2,3,4)
  out <- gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4)
  
  expect_equal(nrow(out), 16)
  expect_equal(ncol(out), 8)
})



test_that("Correct entries in columns", {
  ids <- c(1,3)
  out <- gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4)
  
  expect_equal(out[,1], c(rep(1, 4), rep(0, 4)))
  expect_equal(out[,2], c(rep(0, 4), rep(1, 4)))
  expect_equal(out[,3], c(1, 4, 7, 10, rep(0, 4)))
  expect_equal(out[,4], c(rep(0, 4), 1, 4, 7, 10))
})



test_that("Correct number of rows and columns with quadratic poly_degree 
          parameter", {
  ids <- c(1,2)
  out <- gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4,
                             poly_degree = 2)
  
  expect_equal(nrow(out), 8)
  expect_equal(ncol(out), 6)
})



test_that("Correct number of rows and columns with quadratic poly_degree 
          parameter", {
  ids <- c(1,2)
  out <- gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4,
                             poly_degree = 2)
  
  expect_equal(out[,1], c(rep(1, 4), rep(0, 4)))
  expect_equal(out[,2], c(rep(0, 4), rep(1, 4)))
  expect_equal(out[,3], c(1, 4, 7, 10, rep(0, 4)))
  expect_equal(out[,4], c(rep(0, 4), 1, 4, 7, 10))
  expect_equal(out[,5], c(1, 16, 49, 100, rep(0, 4)))
  expect_equal(out[,6], c(rep(0, 4), 1, 16, 49, 100))
})




