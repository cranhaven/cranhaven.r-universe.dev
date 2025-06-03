test_that("add_intercept handles matrix with existing intercept", {
  mat = matrix(c(1,1,1,2,5,7,1,1,3),nrow=3,ncol=3)
  colnames(mat) = c("(Intercept)", "Var1","Var2")
  result = add_intercept(mat)
  expect_identical(result, mat)
})

test_that("add_intercept handles matrix without intercept", {
  mat = matrix(c(2,3,4,5), nrow=2,ncol=2)
  colnames(mat) = c("Var1","Var2")
  result = add_intercept(mat)
  expected = matrix(c(1, 1, 2, 3, 4, 5), nrow = 2, ncol = 3)
  colnames(expected) = c("(Intercept)", "Var1", "Var2")
  expect_identical(result, expected)
})

test_that("add_intercept handles vector input", {
  vec = c(1,2,3,4)
  result = add_intercept(vec)
  expected = matrix(c(1,1,1,1,1,2,3,4),nrow=4,ncol=2)
  colnames(expected) = c("(Intercept)","Var1")
  expect_identical(result, expected)
})
