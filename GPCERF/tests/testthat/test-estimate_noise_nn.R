test_that("estimate_noise_nn works as expected!", {

  set.seed(425)
  data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

  # Estimate GPS function
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  # Hyperparameter
  hyperparam <- c(0.1, 0.2, 1)
  n_neighbor <- 10
  block_size <- 10000

  # Exposure level
  wi <- 0.4

  # Estimate GPS for the exposure level
  GPS_w <- dnorm(wi,
                 mean = gps_m$gps$e_gps_pred,
                 sd = gps_m$gps$e_gps_std,
                 log = gps_m$used_params$dnorm_log)

  # Order data for easy selection
  coord_obs <- cbind(data$treat, gps_m$gps$gps)
  y_use <- data$Y

  obs_ord <- coord_obs[order(coord_obs[, 1]), ]
  y_use_ord <- y_use[order(coord_obs[, 1])]

  noise <- estimate_noise_nn(hyperparam = hyperparam,
                             w_obs = data$treat,
                             GPS_obs = gps_m$gps$gps,
                             y_obs = y_use_ord,
                             n_neighbor = n_neighbor,
                             nthread = 1)

  expect_equal(length(noise), 1L)
})
