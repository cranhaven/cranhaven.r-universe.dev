test_that("base_GIA", {
  model_params <- c("beta_A" = 1, "beta_B" = 2,
                    "gamma_A" = .5, "gamma_B" = .6,
                    "tau_1" = 1, "tau_2" = 0)
  dose_A <- c(0, 1, 0)
  dose_B <- c(0, 0, 2)
  out <- base_GIA(model_params, dose_A, dose_B)
  exp_out <- c(0, 50, 50)
  expect_equal(out, exp_out)


  ####
  model_params <- c("beta_A" = 1, "beta_B" = 2,
                    "gamma_A" = 0, "gamma_B" = 0,
                    "tau_1" = 1, "tau_2" = 0)
  dose_A <- c(1, 2)
  dose_B <- c(0, 1)
  out <- base_GIA(model_params, dose_A, dose_B)
  exp_out1 <- 50
  exp_out2 <- 50
  expect_equal(out, c(exp_out1, exp_out2))
})


test_that("estimate_GIA", {
  model_params <- c("beta_A" = 1, "beta_B" = 2,
                    "gamma_A" = .5, "gamma_B" = .6,
                    "tau_1" = 1, "tau_2" = 0)
  dose_A <- c(0, 1, 0)
  dose_B <- c(0, 0, 2)
  out <- estimate_GIA(model_params, dose_A, dose_B)
  exp_out <- c(0, 50, 50)
  expect_equal(out, exp_out)
})


test_that("make_grid", {
  n <- 40
  par <- c("beta_A" = 1, "beta_B" = 2)
  out <- make_grid(n = 2, par = par)
  exp_out <- data.frame(dose_A = c(0, 2, 0, 2),
                        dose_B = c(0, 0, 4, 4),
                        rep = 1)
  expect_equal(sum(abs(out -exp_out)), 0)


})


test_that("calc_S_base", {

  best_pars <- c("tau_1" = 0,
                 "tau_2" = 0,
                 "gamma_A" = 1,
                 "gamma_B" = 1)
  out <- calc_S_base(best_pars)
  exp_out <- 1
  expect_equal(exp_out, out)
  #######
  best_pars <- c("tau_1" = -1,
                 "tau_2" = 0,
                 "gamma_A" = 1,
                 "gamma_B" = 1)
  out <- calc_S_base(best_pars)
  exp_out <- .5
  expect_equal(as.numeric(out), exp_out)
  ######
  best_pars <- c("tau_1" = 1,
                 "tau_2" = -4,
                 "gamma_A" = 1,
                 "gamma_B" = 1)
  out <- calc_S_base(best_pars)
  exp_out <- 1
  expect_equal(as.numeric(out), exp_out)


})

test_that("calc_S", {
  best_pars <- c("tau_1" = 0,
                 "tau_2" = 0,
                 "gamma_A" = 1,
                 "gamma_B" = 1)
  out <- calc_S(best_pars)
  exp_out <- 1
  expect_equal(exp_out, out)
  #######
  best_pars <- c("tau_1" = -1,
                 "tau_2" = 0,
                 "gamma_A" = 1,
                 "gamma_B" = 1)
  out <- calc_S(best_pars)
  exp_out <- .5
  expect_equal(as.numeric(out), exp_out)
  ######
  best_pars <- c("tau_1" = 1,
                 "tau_2" = -4,
                 "gamma_A" = 1,
                 "gamma_B" = 1)
  out <- calc_S(best_pars)
  exp_out <- 1
  expect_equal(as.numeric(out), exp_out)



})

test_that("SSE_GIA", {
  best_pars <- c("beta_A" = 1,
                 "beta_B" = 1,
                 "tau_1" = 1,
                 "tau_2" = 0,
                 "gamma_A" = 0,
                 "gamma_B" = 0)
  data <- data.frame(dose_A = c(0, 1),
                     dose_B = c(0, 1),
                     GIA = c(0, 51))

  out <- SSE_GIA(par = best_pars,
                 data = data,
                 GIA_fn = base_GIA,
                 fn_list = NULL)
  exp_out <- 1
  expect_equal(exp_out, out)


})



