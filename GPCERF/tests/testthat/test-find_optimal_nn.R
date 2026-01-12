test_that("find_optimal_nn works as expected!", {
  set.seed(89)
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

  # compute posterior mean and standard deviation for vector of w.
  w <- seq(0, 20, 2)
  design_mt <- model.matrix(~. - 1, data = data[, 3:ncol(data)])
  design_mt <- as.data.frame(design_mt)

  hyperparam_grid <- expand.grid(seq(0.5, 2.5, 1),
                                 seq(0.4, 0.6, 0.2),
                                 seq(0.5, 1.5, 1))

  optimal_cb <- find_optimal_nn(w_obs = data$treat,
                                w = w,
                                y_obs = data$Y,
                                gps_m = gps_m,
                                design_mt = design_mt,
                                hyperparams = hyperparam_grid,
                                n_neighbor = 100,
                                block_size = 2e3)

  all_cb_res <- sapply(optimal_cb, "[[", "cb")
  opt_idx_nn <- order(colMeans(abs(all_cb_res)))[1]
  nn_opt_param <- unlist(hyperparam_grid[opt_idx_nn, ])

  expect_equal(length(optimal_cb), 12L)
  expect_equal(nrow(all_cb_res), 6L)
  expect_equal(ncol(all_cb_res), 12L)
  expect_equal(sum(is.na(all_cb_res)), 0)
})
