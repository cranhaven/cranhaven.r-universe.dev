mod1 <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))
mod2 <- readRDS(test_path("fixtures", "em_fit_with_intercept_only.rds"))

test_that("extract_formula works", {
  expect_equal(extract_formula(mod1), "survival::Surv(y, delta) ~ x")
  expect_equal(extract_formula(mod2), "survival::Surv(y, delta) ~ 1")
})

test_that("nobs works", {
  expect_equal(nobs(mod1), 10000)
  expect_equal(nobs(mod2), 10000)
})

test_that("npredictors works", {
  expect_equal(npredictors(mod1), 2)
  expect_equal(npredictors(mod2), 1)
})

test_that("niterations works", {
  expect_equal(niterations(mod1), 150)
  expect_equal(niterations(mod2), 150)
})

test_that("plot function works when applied to model via EM algorithm", {
  expect_snapshot(plot(mod1))
  expect_snapshot(plot(mod2))
})
