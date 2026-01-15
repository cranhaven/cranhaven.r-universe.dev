test_that("print method works", {
  mod <- readRDS(test_path("fixtures", "ln_fit_with_covariates.rds"))
  expect_snapshot(mod)
})
