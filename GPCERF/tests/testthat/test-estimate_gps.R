test_that("estimate_gps works as expected.", {

  set.seed(651)
  data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

  # Estimate GPS function
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  expect_s3_class(gps_m, "gps")
  expect_equal(length(gps_m$gps), 4L)
  expect_false(gps_m$used_params$dnorm_log)
  expect_equal(gps_m$gps[4, 1], 8.74310154, tolerance = 0.000001)
})
