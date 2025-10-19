test_that("ccc_cpp and lins_ccc_detail_cpp produce expected values", {
  # Test 1: Perfect agreement
  x1 <- c(1, 2, 3, 4, 5)
  mat1 <- cbind(x1, x1)
  ccc_mat <- ccc_cpp(mat1)
  expect_equal(ccc_mat[1, 2], 1.0, tolerance = 1e-12)

  # Test 2: Perfect negative agreement
  x2 <- c(1, 2, 3, 4, 5)
  y2 <- c(5, 4, 3, 2, 1)
  mat2 <- cbind(x2, y2)
  ccc_val2 <- ccc_cpp(mat2)[1, 2]
  expected_r2 <- -1
  mean_diff2 <- mean(x2) - mean(y2)
  var_x2 <- var(x2) * 4 / 5  # biased var
  var_y2 <- var(y2) * 4 / 5
  expected_sxy2 <- expected_r2 * sqrt(var_x2 * var_y2)
  expected_ccc2 <- 2 * expected_sxy2 / (var_x2 + var_y2 + mean_diff2^2)
  expect_equal(ccc_val2, expected_ccc2, tolerance = 1e-12)

  # Test 3: Manually calculated CCC
  x3 <- c(1, 2, 3, 4, 5)
  y3 <- c(1.1, 2.1, 2.9, 3.8, 5.2)
  mean_x3 <- mean(x3)
  mean_y3 <- mean(y3)
  var_x3 <- var(x3) * 4 / 5
  var_y3 <- var(y3) * 4 / 5
  cov_xy3 <- sum((x3 - mean_x3) * (y3 - mean_y3)) / 5
  r3 <- cov_xy3 / sqrt(var_x3 * var_y3)
  sxy3 <- r3 * sqrt(var_x3 * var_y3)
  expected_ccc3 <- 2 * sxy3 / (var_x3 + var_y3 + (mean_x3 - mean_y3)^2)

  mat3 <- cbind(x3, y3)
  fast_ccc3 <- ccc_cpp(mat3)[1, 2]
  expect_equal(fast_ccc3, expected_ccc3, tolerance = 1e-10)
})

test_that("CCC matrix is symmetric", {
  mat <- matrix(rnorm(100 * 4), ncol = 4)
  result <- ccc_cpp(mat)
  expect_equal(result, t(result), tolerance = 1e-12)
})

test_that("CCC detects lack of agreement despite perfect correlation", {
  x <- rnorm(100)
  y <- x * 2 + 5
  mat <- cbind(x, y)
  result <- ccc_cpp(mat)
  expect_true(result[1, 2] < 1)
})

test_that("CCC with CI returns correctly structured result", {
  mat <- matrix(rnorm(100 * 3), ncol = 3)
  result <- ccc_with_ci_cpp(mat)
  expect_named(result, c("est", "lwr.ci", "upr.ci"))
  expect_equal(dim(result$est), dim(result$lwr.ci))
  expect_equal(dim(result$est), dim(result$upr.ci))
})

