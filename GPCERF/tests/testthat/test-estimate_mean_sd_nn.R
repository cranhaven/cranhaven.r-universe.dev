test_that("estimate_mean_sd_nn works as expected!", {

  set.seed(276)
  data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

  # Estimate GPS function
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  # Hyperparameter
  hyperparam <- c(0.1, 0.2, 1)
  n_neighbor <- 15
  block_size <- 10000

  # compute noise
  noise <- estimate_noise_nn(hyperparam = hyperparam,
                             w_obs = data$treat,
                             GPS_obs = gps_m$gps$gps,
                             y_obs = data$Y,
                             n_neighbor = n_neighbor,
                             nthread = 1)

  # compute posterior mean and standard deviation for vector of w.
  w <- seq(0, 20, 1)
  val <- estimate_mean_sd_nn(hyperparam = hyperparam,
                             sigma2 = noise,
                             w_obs = data$treat,
                             w = w,
                             y_obs = data$Y,
                             gps_m = gps_m,
                             n_neighbor = n_neighbor,
                             block_size = block_size)

  expect_equal(length(val), 21)
  expect_equal(val[10], 5.031225, tolerance = 0.0001)
})
