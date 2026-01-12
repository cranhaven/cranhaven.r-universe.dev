test_that("compute_sd_gp works as expected.", {

   set.seed(2384)
   data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)
   w_obs <- obs_exposure <- data$treat

   # Choose an exposure level to compute CERF
   w <- 1.2

   # Define kernel function
   kernel_fn <- function(x) exp(-x^2)

   # Estimate GPS function
   gps_m <- estimate_gps(cov_mt = data[, -(1:2)],
                         w_all = data$treat,
                         sl_lib = c("SL.xgboost"),
                         dnorm_log = FALSE)

   gps <- gps_m$gps$gps

   # set hyperparameters
   hyperparam <- c(0.1, 0.4, 1)
   alpha <- hyperparam[[1]]
   beta <- hyperparam[[2]]
   g_sigma <- hyperparam[[3]]

   # Compute scaled observation data and inverse of covariate matrix.
   scaled_obs <- cbind(obs_exposure * sqrt(1 / beta), gps * sqrt(1 / alpha))

   tentative_sigma <- 0.1

   post_sd <- GPCERF:::compute_sd_gp(w = w,
                                    scaled_obs = scaled_obs,
                                    hyperparam = hyperparam,
                                    sigma = tentative_sigma,
                                    gps_m = gps_m,
                                    kernel_fn = kernel_fn)

   expect_equal(length(post_sd), 1L)
})
