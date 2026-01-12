test_that("estimate_cerf_gp works as expected!", {

  set.seed(129)
  data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

  # Estimate GPS function
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  # exposure values
  w_all <- seq(0, 20, 0.1)

  cerf_gp_obj <- estimate_cerf_gp(data = data,
                                  w = w_all,
                                  gps_m = gps_m,
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1)


  expect_s3_class(cerf_gp_obj, "cerf_gp")

  expect_equal(length(cerf_gp_obj$posterior$mean), 201L)
  expect_equal(length(cerf_gp_obj$posterior$w), 201L)

  # Check non-consistent data and GPS object. ----------------------------------
  # Different size
  set.seed(659)
  data <- generate_synthetic_data(sample_size = 100, gps_spec = 3)
  w_all <- seq(0, 20, 0.1)
  # Estimate GPS function
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  gps_mm <- gps_m
  gps_mm$gps <- gps_mm$gps[1:99, ]

  # exposure values
    expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = data,
                                  w = w_all,
                                  gps_m = gps_mm,
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))

  # Check input parameters -----------------------------------------------------
  set.seed(129)
  data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

  # Estimate GPS function
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  # exposure values
  w_all <- seq(0, 20, 0.1)

  expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = as.matrix(data),
                                  w = w_all,
                                  gps_m = gps_m,
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))

  expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = data,
                                  w = w_all,
                                  gps_m = as.matrix(gps_m),
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))


  expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = data,
                                  w = w_all,
                                  gps_m = gps_m,
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                ggggg_sigma = 1,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))

  expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = data,
                                  w = w_all,
                                  gps_m = gps_m,
                                  params = list(alpha = c(),
                                                beta = c(),
                                                g_sigma = ,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))


  expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = data,
                                  w = w_all,
                                  gps_m = gps_m,
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "at_random"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))

  expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = data,
                                  w = w_all,
                                  gps_m = gps_m,
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "xyz"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))

  # check for data with missing values
  data_na <- data
  data_na$cf1[2] <- NA

  expect_error(cerf_gp_obj <- estimate_cerf_gp(
                                  data = data_na,
                                  w = w_all,
                                  gps_m = gps_m,
                                  params = list(alpha = c(0.1, 0.2, 0.4),
                                                beta = 0.2,
                                                g_sigma = 1,
                                                tune_app = "all"),
                                  outcome_col = "Y",
                                  treatment_col = "treat",
                                  covariates_col = paste0("cf", seq(1,6)),
                                  nthread = 1))


  # Check with two threads -----------------------------------------------------
  set.seed(129)
  data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)

  # Estimate GPS function
  gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                        w_all = data$treat,
                        sl_lib = c("SL.xgboost"),
                        dnorm_log = FALSE)

  # exposure values
  w_all <- seq(0, 30, 0.1)

  cerf_gp_obj_2 <- estimate_cerf_gp(
                                   data = data,
                                   w = w_all,
                                   gps_m = gps_m,
                                   params = list(alpha = c(0.1, 0.2, 0.4),
                                                 beta = 0.2,
                                                 g_sigma = 1,
                                                 tune_app = "all"),
                                   outcome_col = "Y",
                                   treatment_col = "treat",
                                   covariates_col = paste0("cf", seq(1,6)),
                                   nthread = 2)

  expect_s3_class(cerf_gp_obj_2, "cerf_gp")

  expect_equal(length(cerf_gp_obj_2$posterior$mean), 301L)
  expect_equal(length(cerf_gp_obj_2$posterior$w), 301L)
})
