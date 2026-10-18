library(testthat)
library(iglm)

test_that("iglm.data.neighborhood validation works", {
  # Valid empty inputs
  expect_error(iglm.data.neighborhood(matrix(0, nrow = 0, ncol = 2)), NA)
  expect_error(iglm.data.neighborhood(NULL), NA)
  expect_error(iglm.data.neighborhood(list()), NA)
  
  # Invalid empty inputs (wrong ncol)
  expect_error(iglm.data.neighborhood(matrix(0, nrow = 0, ncol = 3)), 
               "Empty neighborhood matrix must have 0 or 2 columns")
  
  # Invalid type
  expect_error(iglm.data.neighborhood("not a matrix"), 
               "`neighborhood` must be a matrix or data frame")
})
