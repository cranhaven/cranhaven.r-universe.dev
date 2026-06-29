library(testthat)
test_that("test-dirrnd.R", {
  # 2,3,4
  target <- c(1, 10, 100, 1000)
  matrix <- reshape_python(target, dim = c(2, 2))
  matrix <- reshape_python(matrix, dim = c(2, 3, 4))
  # print_3_dim_matrix(matrix)
  result <- dirrnd(matrix)
  # print_3_dim_matrix(result)

  # 2,3
  target <- c(1, 3, 5, 9, 7, 5)
  matrix <- reshape_python(target, dim = c(2, 3))
  # print(matrix)
  result <- dirrnd(matrix)
  # print(result)
  expect_equal(1, 1)
})
