test_that("compute_rl_deriv_nn works as expected!", {

  set.seed(325)
  data <- generate_synthetic_data(sample_size = 200)
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  wi <- 12.2

  deriv_val <- compute_rl_deriv_nn(w = wi,
                                   w_obs = data$treat,
                                   gps_m = gps_m,
                                   y_obs = data$Y,
                                   hyperparam = c(0.2, 0.4, 1.2),
                                   n_neighbor = 20,
                                   block_size = 1000)

  expect_equal(length(deriv_val), 1L)
  expect_true(is.matrix(deriv_val))
})
