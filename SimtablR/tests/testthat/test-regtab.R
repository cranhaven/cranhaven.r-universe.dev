library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("regtab fits logistic regression with odds ratios for binary outcome", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex,
    family = stats::binomial("logit")
  ))
  expect_s3_class(tab, "regtab")
  expect_s3_class(tab, "simtab_result")
  expect_true("estimate" %in% names(tab$data))
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab fits linear regression for continuous outcome", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "bmi",
    predictors = ~ age + sex,
    family = stats::gaussian("identity")
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab fits Poisson regression with incidence rate ratios for count outcome", {
  dat <- regtab_fixture()
  tab <- suppressMessages(regtab(
    dat,
    outcomes = "visits_a",
    predictors = ~ age + sex,
    family = stats::poisson("log")
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab supports log-binomial and robust Poisson for prevalence ratios", {
  dat <- regtab_fixture()
  tab <- suppressMessages(regtab(
    dat,
    outcomes = "visits_a",
    predictors = ~ age + sex,
    family = stats::poisson("log"),
    robust = TRUE
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab supports Firth penalized likelihood", {
  skip_if_not_installed("logistf")
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex,
    family = stats::binomial("logit"),
    method = "firth"
  ))
  expect_s3_class(tab, "regtab")
  expect_true(is.numeric(tab$data$estimate))
})

test_that("regtab handles multivariable adjustment with multiple covariates", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex + hypertension,
    family = stats::binomial("logit")
  ))
  expect_true(nrow(tab$data) >= 3)
})

test_that("regtab tidy and glance accessors return valid data frames", {
  tab <- suppressMessages(regtab(
    epitabl,
    outcomes = "adjudicated_acs",
    predictors = ~ age + sex,
    family = stats::binomial("logit")
  ))
  tidy_df <- generics::tidy(tab)
  glance_df <- generics::glance(tab)
  expect_s3_class(tidy_df, "data.frame")
  expect_s3_class(glance_df, "data.frame")
})

test_that("regtab validates input families and throws classed errors on invalid inputs", {
  expect_error(regtab(epitabl, outcomes = "adjudicated_acs", predictors = ~ nonexistent_col))
})

test_that("regtab marks a constant outcome as failed instead of reporting 1.00", {
  dat <- epitabl
  dat$constant_outcome <- factor("No", levels = c("No", "Yes"))

  expect_warning(
    fit <- suppressMessages(regtab(
      dat, c("constant_outcome", "rehospitalized"), ~ age + sex,
      family = binomial()
    )),
    "takes a single value"
  )
  info <- model_info(fit)
  expect_true(info$failed[info$outcome == "constant_outcome"])
  expect_false(info$failed[info$outcome == "rehospitalized"])
  expect_false("constant_outcome" %in% fit$data$outcome)
  expect_output(print(fit), "Failed outcomes: constant_outcome")

  # A verb on the result recomputes through the same check.
  expect_warning(refit <- adjust(fit, smoking), "takes a single value")
  expect_true(model_info(refit)$failed[model_info(refit)$outcome == "constant_outcome"])

  # A lone constant outcome cannot produce a table at all.
  expect_error(
    suppressWarnings(regtab(dat, "constant_outcome", ~ age, family = binomial())),
    class = "simtab_error_engine"
  )
})
