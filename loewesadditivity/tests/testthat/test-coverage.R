test_that("simulate coverage works", {
  df <- loewesadditivity::cyrpa_ripr
  df$dose_A <- df$CyRPA
  df$dose_B <- df$RIPR
  data <- fortify_gia_data(df)
  ##
  model_params <- c("beta_A" = .247, "beta_B" = .224,
                    "gamma_A" = .734, "gamma_B" = .806,
                    "tau_1" = .28, "tau_2" = -.28)
  experimental_grid <- make_grid(par = model_params,
                                 n = 5)
  n_boot <- 3
  n_sims <- 5
  GIA_fn <- base_GIA
  S_fn <- calc_S_base
  fn_list <- NULL
  alpha <- .05
  verbose <- TRUE
  out <- simulate_coverage(n_sims = n_sims,
                         n_boot = n_boot,
                         verbose = FALSE,
                         experimental_grid = experimental_grid,
                         model_par = model_params,
                         alpha = .05,
                         noise_par = c("a0" = 3, "a1" = .01),
                         GIA_fn = base_GIA,
                         fn_list = NULL)
  expect_equal(length(out), 4)
})
