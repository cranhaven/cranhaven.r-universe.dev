test_that("boot", {
  model_params <- c("beta_A" = 1, "beta_B" = 2,
                    "gamma_A" = .5, "gamma_B" = .6,
                    "tau_1" = 1, "tau_2" = 0)
  dose_A <- c(0, 1, 0)
  dose_B <- c(0, 0, 2)
  GIA <- c(0,49, 51)
  out <- boot_GIA(par = model_params,
                  gia_df = data.frame(dose_A = dose_A,
                                      dose_B = dose_B,
                                      GIA = GIA),
                  gia_est = c(0, 50, 50),
                  n_boot = 10)

  expect_true(length(out) == 3)
})
