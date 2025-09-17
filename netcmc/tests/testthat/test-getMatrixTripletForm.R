context("getTripletForm")

test_that("test getTripletForm can return the expected matrix. Note that the c++ function uses 0 indexing.", {
  
  matrix = matrix(c(0, 3, 0, 0, 0, 1, 1, 0, 0), 3, 3)
  
  MatrixTripletForm = getTripletForm(matrix)
  actualMatrixTripletForm = matrix(c(0, 1, 2, 2, 0, 1, 1, 3, 1), 3, 3)
  
  expect_equal(MatrixTripletForm, actualMatrixTripletForm)
  
})

test_that("test getTripletForm can return the expected matrix. Note that the c++ function uses 0 indexing. Second version.", {
  
  matrix = matrix(c(0, 3, 0, 0, 0, 1, -1, 0, 0), 3, 3)
  
  MatrixTripletForm = getTripletForm(matrix)
  actualMatrixTripletForm = matrix(c(0, 1, 2, 2, 0, 1, -1, 3, 1), 3, 3)
  
  expect_equal(MatrixTripletForm, actualMatrixTripletForm)
  
})

test_that("test getTripletForm can return the expected matrix. Note that the c++ function uses 0 indexing. Third version.", {
  
  matrix = matrix(c(0, 3, 0, 0, 0, 1, -0.1, 0, 0), 3, 3)
  
  MatrixTripletForm = getTripletForm(matrix)
  actualMatrixTripletForm = matrix(c(0, 1, 2, 2, 0, 1, -0.1, 3, 1), 3, 3)
  
  expect_equal(MatrixTripletForm, actualMatrixTripletForm)
  
})
