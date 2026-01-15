mod_spec <- parsnip::survival_reg(mode = "censored regression") |>
  parsnip::set_engine("survival_ln_mixture_em", starting_seed = 10, iter = 150)

f_fit <- parsnip::fit(mod_spec, survival::Surv(y, delta) ~ x,
  data = sim_data$data
)

mod <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))

test_that("parsnip specification works", {
  expect_equal(f_fit$fit, mod, tolerance = 1)
})

test_that("parsnip survival prediction works", {
  new_data <- data.frame(x = c("0", "1"))
  pred <- predict(mod, new_data = new_data, type = "survival", eval_time = c(20, 100))
  expected <- predict(f_fit, new_data = new_data, type = "survival", eval_time = c(20, 100))

  expect_equal(pred, expected, tolerance = 1)
})

test_that("parsnip hazard prediction works", {
  new_data <- data.frame(x = c("0", "1"))
  pred <- predict(mod, new_data = new_data, type = "hazard", eval_time = c(20, 100))
  expected <- predict(f_fit, new_data = new_data, type = "hazard", eval_time = c(20, 100))

  expect_equal(pred, expected, tolerance = 1)
})
