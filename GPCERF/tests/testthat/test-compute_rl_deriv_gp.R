test_that("compute_rl_deriv_gp works as expected!", {

  set.seed(127)
  data <- generate_synthetic_data(sample_size = 200)
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  wi <- 8.6

  deriv_val <- compute_rl_deriv_gp(w = wi,
                                   w_obs = data$treat,
                                   y_obs = data$Y,
                                   gps_m = gps_m,
                                   hyperparam = c(0.2, 0.8, 2))

  expect_equal(length(deriv_val), 1L)
  expect_true(is.matrix(deriv_val))

})
