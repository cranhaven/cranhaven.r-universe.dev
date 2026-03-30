test_shrinkTPR <- function(args, eval_points = c(-2, 0, 2), log_pred = FALSE) {

  if (!torch::torch_is_installed()) {
    skip("Torch is not installed. Skipping test.")
  }

  set.seed(123)
  torch_manual_seed(123)

  # Create mock data
  full_dat <- data.frame(
    y = sin(2 * pi * runif(20)) + rnorm(20, sd = 0.1),
    x1 = runif(20),
    x2 = rnorm(20)
  )
  train <- full_dat[1:15, ]
  test <- full_dat[16:20, ]
  args$data <- train
  args$formula <- y ~ x1 + x2

  # Fit model
  res <- do.call(shrinkTPR, args)

  # Test model object
  expect_s3_class(res, "shrinkTPR")
  expect_true("shrinkTPR" %in% class(res))

  # Test prediction methods
  preds <- predict(res, newdata = test)
  expect_type(preds, "double")
  expect_equal(dim(preds), c(100, nrow(test)))  # Default nsamp for prediction functions

  # Test LPDS
  lpds <- LPDS(res, data_test = test[1, ])
  expect_type(lpds, "double")
  expect_length(lpds, 1)

  # Test predictive density evaluation
  pred_dens <- eval_pred_dens(eval_points, res, data_test = test[1, ], log = log_pred)
  expect_type(pred_dens, "double")
  expect_length(pred_dens, length(eval_points))

  # Test predictive moments
  moments <- calc_pred_moments(res, newdata = test, nsamp = 100)
  expect_type(moments, "list")
  expect_named(moments, c("means", "vars"))
  expect_equal(dim(moments$means), c(100, nrow(test)))
  expect_equal(dim(moments$vars), c(100, nrow(test), nrow(test)))

  # Test posterior samples
  posterior <- gen_posterior_samples(res, nsamp = 100)
  expect_type(posterior, "list")
  names_posterior <- c("thetas", "sigma2", "lambda", "nu")
  if (res$model_internals$x_mean) {
    names_posterior <- c(names_posterior[1:3], "betas", "lambda_mean", names_posterior[4])
  }
  expect_named(posterior, names_posterior)
  expect_equal(nrow(posterior$thetas), 100)

  # Test marginal generation (1D)
  marg1 <- gen_marginal_samples(res, to_eval = "x1", nsamp = 10, n_eval_points = 10)
  expect_type(marg1, "list")
  expect_true(all(c("mean_pred", "grid") %in% names(marg1)))
  expect_equal(dim(marg1$mean_pred), c(10, 10))
  expect_length(marg1$grid, 10)
  expect_s3_class(marg1, "shrinkGPR_marg_samples_1D")

  # Test marginal generation (2D)
  marg2 <- gen_marginal_samples(res, to_eval = c("x1", "x2"), nsamp = 5, n_eval_points = 5)
  expect_type(marg2, "list")
  expect_true(all(c("mean_pred", "grid") %in% names(marg2)))
  expect_equal(dim(marg2$mean_pred), c(5, 5, 5))
  expect_type(marg2$grid, "list")
  expect_length(marg2$grid, 2)
  expect_s3_class(marg2, "shrinkGPR_marg_samples_2D")

  # Test plotting method (1D)
  if (requireNamespace("shrinkTVP", quietly = TRUE)) {
    expect_silent(plot(marg1))
  } else {
    expect_error(plot(marg1), "The 'shrinkTVP' package is required")
  }

  # Test plotting method (2D)
  if (requireNamespace("plotly", quietly = TRUE)) {
    p <- plot(marg2)
    expect_s3_class(p, "plotly")
  } else {
    expect_error(plot(marg2), "The 'plotly' package is required")
  }

  if (res$model_internals$x_mean) {
    expect_true(all(c("betas", "lambda_mean") %in% names(posterior)))
  }
}

# Define scenarios
scenarios <- expand.grid(
  auto_stop = c(TRUE, FALSE),
  kernel = c(kernel_se, kernel_matern_32),
  flow = c(sylvester)
)
names(scenarios) <- c("auto_stop", "kernel", "flow")

# Parameter toggles
params <- c("display_progress", "auto_stop")

for (i in seq_len(nrow(scenarios))) {
  for (j in params) {
    args <- formals(shrinkGPR)
    args <- args[sapply(args, function(x) !is.null(x))]

    args[[j]] <- !args[[j]]
    args$auto_stop <- scenarios$auto_stop[i]
    args$kernel_func <- scenarios$kernel[[i]]
    args$flow_func <- scenarios$flow[[i]]
    args$n_epochs <- 10  # Limit epochs for testing

    test_that(paste0(
      "scenario: ", i,
      ", auto_stop: ", scenarios$auto_stop[i],
      ", toggled: ", j
    ), {
      test_shrinkTPR(args)

      # Test also with mean equation
      args$formula_mean <- as.formula(~ x1 + x2)
      test_shrinkTPR(args)
    })
  }
}
