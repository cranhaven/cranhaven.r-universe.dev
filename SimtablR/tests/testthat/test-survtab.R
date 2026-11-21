library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("survtab fits basic Cox proportional hazards model", {
  skip_if_not_installed("survival")
  fit <- suppressMessages(survtab(
    epitabl,
    time = mace_time_days,
    event = mace_event,
    predictors = ~ renal_impairment
  ))
  expect_s3_class(fit, "simtab_cox")
  expect_s3_class(fit, "simtab_result")
  expect_true(is.numeric(fit$data$terms$estimate))
})

test_that("survtab supports multivariable Cox regression", {
  skip_if_not_installed("survival")
  fit <- suppressMessages(survtab(
    epitabl,
    time = mace_time_days,
    event = mace_event,
    predictors = ~ renal_impairment + age + sex
  ))
  expect_s3_class(fit, "simtab_cox")
  expect_true(nrow(fit$data$terms) >= 3)
})

test_that("survtab supports stratification in Cox model via strata()", {
  skip_if_not_installed("survival")
  fit <- suppressMessages(survtab(
    epitabl,
    time = mace_time_days,
    event = mace_event,
    predictors = ~ renal_impairment + survival::strata(sex)
  ))
  expect_s3_class(fit, "simtab_cox")
})

test_that("survtab tidy and glance accessors return summary statistics", {
  skip_if_not_installed("survival")
  fit <- suppressMessages(survtab(
    epitabl,
    time = mace_time_days,
    event = mace_event,
    predictors = ~ renal_impairment
  ))
  tidy_df <- generics::tidy(fit)
  glance_df <- generics::glance(fit)
  expect_s3_class(tidy_df, "data.frame")
  expect_s3_class(glance_df, "data.frame")
})

test_that("survtab validates survival outcome formula and raises classed errors", {
  skip_if_not_installed("survival")
  expect_error(survtab(epitabl, time = nonexistent, event = mace_event, predictors = ~ age))
})

