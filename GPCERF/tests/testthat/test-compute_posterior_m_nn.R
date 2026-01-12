test_that("compute_posterior_m_nn works as expected.", {

  set.seed(209)
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
  gps_w <- dnorm(wi,
                 mean = gps_m$gps$e_gps_pred,
                 sd = gps_m$gps$e_gps_std,
                 log = gps_m$used_params$dnorm_log)

  # Order data for easy selection
  coord_obs <- cbind(data$treat, gps_m$gps$gps)
  y_use <- data$Y

  obs_ord <- coord_obs[order(coord_obs[, 1]), ]
  y_use_ord <- y_use[order(coord_obs[, 1])]

  val <- compute_posterior_m_nn(hyperparam = hyperparam,
                                w = wi,
                                gps_w = gps_w,
                                obs_ord = obs_ord,
                                y_obs_ord = y_use_ord,
                                n_neighbor = n_neighbor,
                                block_size = block_size)


  expect_equal(nrow(val), 21L)
  expect_equal(ncol(val), 2L)

  # testing getting the same results with different block size

  val_1 <- compute_posterior_m_nn(hyperparam = hyperparam,
                                  w = wi,
                                  gps_w = gps_w,
                                  obs_ord = obs_ord,
                                  y_obs_ord = y_use_ord,
                                  n_neighbor = n_neighbor,
                                  block_size = 40)

  val_2 <- compute_posterior_m_nn(hyperparam = hyperparam,
                                  w = wi,
                                  gps_w = gps_w,
                                  obs_ord = obs_ord,
                                  y_obs_ord = y_use_ord,
                                  n_neighbor = n_neighbor,
                                  block_size = 50)

  val_3 <- compute_posterior_m_nn(hyperparam = hyperparam,
                                  w = wi,
                                  gps_w = gps_w,
                                  obs_ord = obs_ord,
                                  y_obs_ord = y_use_ord,
                                  n_neighbor = n_neighbor,
                                  block_size = 65)

  val_4 <- compute_posterior_m_nn(hyperparam = hyperparam,
                                  w = wi,
                                  gps_w = gps_w,
                                  obs_ord = obs_ord,
                                  y_obs_ord = y_use_ord,
                                  n_neighbor = n_neighbor,
                                  block_size = 10000)

  expect_equal(val_1[15, 2], val_2[15, 2], tolerance = 0.000001)
  expect_equal(val_1[15, 2], val_3[15, 2], tolerance = 0.000001)
  expect_equal(val_1[15, 2], val_4[15, 2], tolerance = 0.000001)
  expect_equal(val_2[12, 2], val_1[12, 2], tolerance = 0.000001)
  expect_equal(val_2[12, 2], val_3[12, 2], tolerance = 0.000001)
  expect_equal(val_2[12, 2], val_4[12, 2], tolerance = 0.000001)

})
