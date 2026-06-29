library(testthat)
test_that("test-reshape_python.R", {
  # [1 2
  #  3 4] ->
  # [[1 2
  #   3 4]]
  matrix <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  trans_prior <- reshape_python(matrix, dim = c(1, 2, 2))
  expected_trans_prior <- array(c(1, 3, 2, 4), dim = c(1, 2, 2))
  expect_equal(trans_prior, expected_trans_prior)
  # [1 2 3 4] ->
  # [[1 2
  #   3 4]]
  trans_prior <- reshape_python(c(1, 2, 3, 4), dim = c(1, 2, 2))
  expected_trans_prior <- array(c(1, 3, 2, 4), dim = c(1, 2, 2))
  expect_equal(trans_prior, expected_trans_prior)
})
