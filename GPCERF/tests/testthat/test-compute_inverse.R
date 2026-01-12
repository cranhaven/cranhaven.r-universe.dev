test_that("compute_inverse works as expected.", {

  set.seed(1934)
  A <- runif(10)
  B <- runif(10)
  C <- cbind(A, B)
  kernel_fn <- function(x) exp(-x ^ 2)
  D <- kernel_fn(as.matrix(dist(C)))
  inv_sigma_obs <- compute_inverse(D)

  expect_true(is.matrix(inv_sigma_obs))
  expect_equal(inv_sigma_obs[1, 1], 1589.723863, tolerance = 0.00001)
  expect_equal(nrow(inv_sigma_obs), 10L)
  expect_equal(ncol(inv_sigma_obs), 10L)

  data_frame <- data.frame(runif(100))
  expect_error(compute_inverse(data_frame))

  non_sq_mat <- matrix(runif(100), ncol = 20)
  expect_error(compute_inverse(non_sq_mat))
})
