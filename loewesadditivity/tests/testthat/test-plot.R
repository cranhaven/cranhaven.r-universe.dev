

test_that("plot_surface", {
  df <- loewesadditivity::cyrpa_ripr
  df$dose_A <- df$CyRPA
  df$dose_B <- df$RIPR
  data <- fortify_gia_data(df)
  model_params <- c(
    "beta_A" = .5,
    "beta_B" = .5,
    "gamma_A" = .5,
    "gamma_B" = .5,
    "tau_1" = 0,
    "tau_2" = 0
  )
  n_boot <- 10
  GIA_fn <- base_GIA
  S_fn <- calc_S_base
  fn_list <- NULL
  alpha <- .05
  verbose <- FALSE
  out_surface <- estimate_params(
    data = data,
    init_params = model_params,
    n_boot = n_boot,
    GIA_fn = GIA_fn,
    S_fn = S_fn,
    fn_list = fn_list,
    alpha = alpha,
    verbose = verbose
  )
  out2 <- plot_surface(out_surface)
  expect_true(length(out2) > 0)
})


test_that("plot_curves", {
  df <- loewesadditivity::cyrpa_ripr
  df$dose_A <- df$CyRPA
  df$dose_B <- df$RIPR
  data <- fortify_gia_data(df)
  model_params <- c(
    "beta_A" = .5,
    "beta_B" = .5,
    "gamma_A" = .5,
    "gamma_B" = .5,
    "tau_1" = 0,
    "tau_2" = 0
  )
  n_boot <- 10
  GIA_fn <- base_GIA
  S_fn <- calc_S_base
  fn_list <- NULL
  alpha <- .05
  verbose <- FALSE
  out <- estimate_params(
    data = data,
    init_params = model_params,
    n_boot = n_boot,
    GIA_fn = GIA_fn,
    S_fn = S_fn,
    fn_list = fn_list,
    alpha = alpha,
    verbose = verbose
  )
  out2 <- plot_curves(out)
  expect_true(length(out2) > 0)
})


test_that("plot_isobolograms", {
  df <- loewesadditivity::cyrpa_ripr
  df$dose_A <- df$CyRPA
  df$dose_B <- df$RIPR
  data <- fortify_gia_data(df)
  model_params <- c(
    "beta_A" = .5,
    "beta_B" = .5,
    "gamma_A" = .5,
    "gamma_B" = .5,
    "tau_1" = 0,
    "tau_2" = 0
  )
  n_boot <- 10
  GIA_fn <- base_GIA
  S_fn <- calc_S_base
  fn_list <- NULL
  alpha <- .05
  verbose <- FALSE
  out_iso <- estimate_params(
    data = data,
    init_params = model_params,
    n_boot = n_boot,
    GIA_fn = GIA_fn,
    S_fn = S_fn,
    fn_list = fn_list,
    alpha = alpha,
    verbose = verbose
  )


  out2 <- plot_isobologram(out_iso)
  expect_true(length(out2) > 0)
})

