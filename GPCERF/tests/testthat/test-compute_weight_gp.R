test_that("multiplication works", {

  set.seed(917)
  # Generate synthetic data
  data <- generate_synthetic_data(sample_size = 200, gps_spec = 3)
  w_obs <- obs_exposure <- data$treat

  # Choose an exposure level to compute CERF
  w <- 1.8

  # Define kernel function
  kernel_fn <- function(x) exp(-x ^ 2)

  # compute GPS, e_gps_pred, and e_gps_std
  e_gps <- xgboost(label = data$treat,
                   data = as.matrix(data[, -(1:2)]),
                   nrounds = 50)
  e_gps_pred <- predict(e_gps, as.matrix(data[, -(1:2)]))
  e_gps_std <- sd(data$treat - e_gps_pred)
  gps <- dnorm(data$treat, mean = e_gps_pred, sd = e_gps_std, log = TRUE)

  gps_m <- list()
  gps_m$gps <- data.frame(gps, e_gps_pred, e_gps_std)
  gps_m$used_params$dnorm_log <- TRUE

  # set hyperparameters
  hyperparam <- c(0.1, 0.4, 1)
  alpha <- hyperparam[1]
  beta <- hyperparam[2]
  g_sigma <- hyperparam[3]

  # Compute scaled observation data and inverse of covariate matrix.
  scaled_obs <- cbind(obs_exposure * sqrt(1 / beta), gps * sqrt(1 / alpha))
  colnames(scaled_obs) <- c("w_sc_obs", "gps_sc_obs")

  sigma_obs <- g_sigma * kernel_fn(as.matrix(dist(scaled_obs))) +
                        diag(nrow(scaled_obs))
  inv_sigma_obs <- compute_inverse(sigma_obs)

  weight <- compute_weight_gp(w = w,
                              w_obs = w_obs,
                              scaled_obs = scaled_obs,
                              hyperparam = hyperparam,
                              inv_sigma_obs = inv_sigma_obs,
                              gps_m = gps_m,
                              kernel_fn = kernel_fn)

  expect_equal(length(weight$weight), 200L)
  expect_equal(weight$weight[28], 0.0002182767, tolerance = 10e-5)
})
