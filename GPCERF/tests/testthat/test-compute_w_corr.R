test_that("Weighted correlation works as expected.", {

  set.seed(239)
  data1 <- generate_synthetic_data(sample_size = 100)
  weights1 <- runif(nrow(data1))
  val1 <- compute_w_corr(w = data1$treat,
                         covariate = data1[, 3:ncol(data1)],
                         weight = weights1)

  expect_vector(val1$absolute_corr)
  expect_equal(length(val1), 4L)
  expect_equal(length(val1$absolute_corr), 6L)
  expect_equal(val1$absolute_corr[["cf1"]], 0.177344, tolerance = 0.00001)
  expect_equal(val1$absolute_corr[["cf2"]], 0.3641282, tolerance = 0.00001)

  expect_error(compute_w_corr(w = data1$treat,
                              covariate = as.matrix(data1[, 3:ncol(data1)]),
                              weight = weights1))

  expect_error(compute_w_corr(w = data1$treat[1:90],
                              covariate = data1[, 3:ncol(data1)],
                              weight = weights1))


  # number of data.samples and weights should be the same
  data3 <- generate_synthetic_data(sample_size = 50)
  weights3 <- runif(nrow(data3) + 20)
  expect_error(compute_w_corr(w = data3$treat,
                              covariate = data3[, 3:ncol(data3)],
                              weight = weights3))
})
