# Filename: test-functions.R
# Date: 31.12.2025
# Author: Felix Willems

# function: getColRowVal()
test_that("getColRowVal() works as expected)", {
  X <- matrix(c(1, 2, 3,
                4, 5, 6,
                7, 8, 9), 3, 3)
  rc <- sample(1:97, 2)
  jix <- getColRowVal(X, rc[1] - 1, rc[2] - 1)
  M <- Matrix::sparseMatrix(i = jix[[2]],
                            j = jix[[1]],
                            x = jix[[3]],
                            dims = c(100, 100))
  expect_identical(as.matrix(M[rc[2]:(rc[2] + 2), rc[1]:(rc[1] + 2)]), X)
})

# function: optDeg()
test_that("optDeg() works as expected)", {
  v1 <- as.vector(c(0, 1, 0))
  v2 <- as.vector(c(1, 0, 1))
  expect_equal(optDeg(v1, v2), 90)
  v1 <- as.vector(c(0, 1))
  v2 <- as.vector(c(1, 1))
  expect_equal(optDeg(v1, v2), 45)
  v1 <- as.vector(c(0, 0, 1))
  v2 <- as.vector(c(1, 1, 1))
  expect_equal(optDeg(v1, v2), atan(sqrt(2)) * 180 / pi)
})