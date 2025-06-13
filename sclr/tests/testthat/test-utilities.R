# Tests of the general utility functions
# Arseniy Khvorov
# Created 2019/10/18
# Last edit 2019/10/18

library(sclr)

test_that("parameter names are created appropriately", {
  x <- model.matrix(status ~ logHI, one_titre_data)
  expect_equal(get_par_names(x), c("theta", "beta_0", "beta_logHI"))
  expect_equal(
    get_par_names(x, conventional_names = TRUE),
    c("(Baseline)", "(Intercept)", "logHI")
  )
  x <- model.matrix(status ~ 1, one_titre_data)
  expect_equal(get_par_names(x), c("theta", "beta_0"))
})

test_that("x_coeffs are calculated", {
  n <- 10
  x <- matrix(c(rep(1, n), rnorm(n), rnorm(n)), nrow = n, ncol = 3)
  x_coeffs <- get_x_coeffs(x)
  expect_equal(x[, 1] * x[, 1], x_coeffs[, 1])
  expect_equal(x[, 1] * x[, 2], x_coeffs[, 2])
  expect_equal(x[, 1] * x[, 3], x_coeffs[, 3])
  expect_equal(x[, 2] * x[, 2], x_coeffs[, 4])
  expect_equal(x[, 2] * x[, 3], x_coeffs[, 5])
  expect_equal(x[, 3] * x[, 3], x_coeffs[, 6])
})

test_that("symmetrical matrix is created", {
  vec <- c(0, 1, 2, 3, 4, 5)
  expected_matrix <- matrix(c(0, 1, 2, 1, 3, 4, 2, 4, 5), ncol = 3)
  expect_equal(build_symm_mat(vec), expected_matrix)
  vecs <- data.frame(v1 = c(0, 1), v2 = c(4, 5), v3 = c(7, 8))
  build_symm_mat(vecs)
  expected_out <- list(
    `1` = matrix(c(0, 4, 4, 7), ncol = 2),
    `2` = matrix(c(1, 5, 5, 8), ncol = 2)
  )
  expect_equal(build_symm_mat(vecs), expected_out)
})

test_that("symmetrical matrix throws error with unknown input", {
  expect_error(build_symm_mat(NULL))
})

test_that("dimensions of a symmetrical matrix are calculated", {
  expect_error(get_symm_dims(2))
  expect_equal(get_symm_dims(3), 2)
  expect_error(get_symm_dims(4))
  expect_equal(get_symm_dims(6), 3)
})
