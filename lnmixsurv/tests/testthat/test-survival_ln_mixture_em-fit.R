test_that("outcome must be Surv object", {
  expect_error(
    survival_ln_mixture_em(y ~ x, sim_data$data)
  )
})

test_that("only right censored data supported", {
  expect_error(
    survival_ln_mixture_em(survival::Surv(y, delta, type = "left") ~ x, sim_data$data)
  )
})

test_that("events at time zero are not supported", {
  data_new <- sim_data$data
  data_new$y[1] <- 0
  expect_error(
    survival_ln_mixture_em(survival::Surv(y, delta, type = "left") ~ x, sim_data$data)
  )
})

test_that("when using ~NULL, intercept must be TRUE", {
  expect_error(
    survival_ln_mixture_em(survival::Surv(y, delta) ~ NULL, sim_data$data, intercept = FALSE)
  )
})

test_that("survival_ln_mixture_em doesn't work with xy specification", {
  expect_error(
    survival_ln_mixture_em(sim_data$data$y, sim_data$data$x)
  )
})

test_that("survival_ln_mixture_em works with intercept only fit", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_intercept_only.rds"))
  expect_equal(tidy(mod)$estimate, c(4.06, 3.86), tolerance = 1)
})

test_that("fit works as expected with simulated data", {
  mod <- survival_ln_mixture_em(survival::Surv(y, delta) ~ x, sim_data$data, 
                                starting_seed = 10, 
                                iter = 150)
  
  mod_tidy <- tidy(mod, effects = c("fixed", "auxiliary"))
  
  expected_result <- structure(
    list(term = c(
      "(Intercept)_1", "x1_1", "(Intercept)_2",
      "x1_2", "phi_1", "phi_2", "eta_1", "eta_2"
    ), estimate = c(
      4.01 , -1.54, 3.78, 0.64, 22.8,
      2.57, 4.9e-14, 1
    )),
    row.names = c(NA, -8L), 
    class = c("tbl_df", "tbl", "data.frame"))
  
  expect_equal(mod$nobs, 10000)
  expect_equal(mod_tidy, expected_result, tolerance = 1)
})
