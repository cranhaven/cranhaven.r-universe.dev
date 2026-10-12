test_that("estimate_params", {
  model_params <- c("beta_A" = 1, "beta_B" = 2,
                    "gamma_A" = .5, "gamma_B" = .6,
                    "tau_1" = 1, "tau_2" = 0)
  dose_A <- c(0, 1, 0)
  dose_B <- c(0, 0, 2)
  GIA <- c(0, 49, 51)
  data <- data.frame(dose_A = dose_A,
                     dose_B = dose_B,
                     GIA = GIA)
  n_boot <- 100
  GIA_fn <- base_GIA
  S_fn <- calc_S_base
  fn_list <- NULL
  alpha <- .05
  verbose <- FALSE
  out <- estimate_params(data = data,
                         init_params = model_params,
                         n_boot = n_boot,
                         GIA_fn = GIA_fn,
                         S_fn = S_fn,
                         fn_list = fn_list,
                         alpha = alpha,
                         verbose = verbose)
  expect_equal(length(out), 4)



  df <- loewesadditivity::cyrpa_ripr
  df$dose_A <- df$CyRPA
  df$dose_B <- df$RIPR
  data <- fortify_gia_data(df)
  ##
  model_params <- c("beta_A" = .5, "beta_B" = .5,
                    "gamma_A" = .5, "gamma_B" = .5,
                    "tau_1" = 0, "tau_2" = 0)
  n_boot <- 10
  GIA_fn <- base_GIA
  S_fn <- calc_S_base
  fn_list <- NULL
  alpha <- .05
  verbose <- FALSE
  out <- estimate_params(data = data,
                         init_params = model_params,
                         n_boot = n_boot,
                         GIA_fn = GIA_fn,
                         S_fn = S_fn,
                         fn_list = fn_list,
                         alpha = alpha,
                         verbose = verbose)
  expect_equal(length(out), 4)



})
