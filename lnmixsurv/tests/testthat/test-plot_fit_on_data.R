data_model <- tibble::tibble(sim_data$data)

test_that("plot_fit_on_data works for Bayesian model with covariates", {
  mod <- readRDS(test_path("fixtures", "ln_fit_with_covariates.rds"))
  
  expect_snapshot(plot_fit_on_data(mod, data_model, interval = 'credible', level = 0.95,
                                    type = 'survival')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, interval = 'credible', level = 0.95,
                                    type = 'hazard')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'survival')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'hazard')$preds)
})

test_that("plot_fit_on_data works for Bayesian model with intercept only", {
  mod <- readRDS(test_path("fixtures", "ln_fit_with_intercept_only.rds"))
  
  expect_snapshot(plot_fit_on_data(mod, data_model, interval = 'credible', level = 0.95,
                                    type = 'survival')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, interval = 'credible', level = 0.95,
                                    type = 'hazard')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'survival')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'hazard')$preds)
})

test_that("plot_fit_on_data works for EM model with covariates", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'survival')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'hazard')$preds)
})

test_that("plot_fit_on_data works for EM model with intercept only", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_intercept_only.rds"))
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'survival')$preds)
  
  expect_snapshot(plot_fit_on_data(mod, data_model, type = 'hazard')$preds)
})
