test_that("compute_deriv_weights_gp works as expected!", {

  set.seed(9615)
  data <- generate_synthetic_data(sample_size = 200)
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  wi <- 4.2
  weights <- compute_deriv_weights_gp(w = wi,
                                      w_obs = data$treat,
                                      gps_m = gps_m,
                                      hyperparam = c(1, 1, 2))

  expect_equal(length(weights), nrow(data))
  expect_equal(weights[37], 3.307249e-05, tolerance = 0.00001)
})
