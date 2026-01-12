test_that("compute_m_sigma works as expected!", {

   set.seed(1282)
   data <- generate_synthetic_data(sample_size = 250, gps_spec = 3)

   w_all <- seq(0, 20, 0.1)

   #Estimate GPS function
   gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                         w_all = data$treat,
                         sl_lib = c("SL.xgboost"),
                         dnorm_log = FALSE)

   outcome_data <- data[["Y"]]
   treatment_data <- data[["treat"]]
   covariates_data <- data[, paste0("cf", seq(1,6)), drop=FALSE]


   tune_res <- compute_m_sigma(hyperparam = c(0.09, 0.09, 10),
                               outcome_data = outcome_data,
                               treatment_data = treatment_data,
                               covariates_data = covariates_data,
                               w = w_all,
                               gps_m = gps_m,
                               tuning = FALSE)

   gp_cerf <- tune_res$est

   expect_equal(length(gp_cerf), 201L)
   expect_equal(length(w_all), 201L)
   expect_vector(gp_cerf)
   expect_equal(gp_cerf[10], 2.105425, tolerance = 0.000001)

   tune_res_t <- compute_m_sigma(hyperparam = c(0.09, 0.09, 10),
                                 outcome_data = outcome_data,
                                 treatment_data = treatment_data,
                                 covariates_data = covariates_data,
                                 w = w_all,
                                 gps_m = gps_m,
                                 tuning = TRUE)

   expect_equal(length(tune_res_t$cb), 6L)
   expect_equal(length(tune_res_t$est), 201L)
   expect_true(is.na(tune_res_t$est[1]))
})
