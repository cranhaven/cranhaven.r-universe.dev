library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("sensitivity computes unmeasured confounding bounds and variations", {
  tab <- suppressMessages(table1(epitabl, "hypertension", by = "adjudicated_acs", measure = "PR", ref = "No"))
  sens <- suppressMessages(sensitivity(tab, denominator = "complete"))
  expect_s3_class(sens, "simtab_sensitivity")
  expect_s3_class(sens, "simtab_report")
})

test_that("e_value calculates E-values from simtab_result", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "rr"))
  ev <- e_value(tab)
  expect_s3_class(ev, "simtab_e_value")
  expect_s3_class(ev, "simtab_result")
  expect_true(is.numeric(ev$data$e_value))
})

test_that("e_value on a tb result skips reference rows and names levels", {
  tab <- suppressMessages(tb(epitabl, smoking, diabetes, or = TRUE, ref = "Never"))
  ev <- e_value(tab)
  expect_identical(nrow(ev$data), 2L)
  expect_setequal(ev$data$term, c("smoking: Former", "smoking: Current"))
  expect_false(any(ev$data$estimate == 1 & is.na(ev$data$conf.low)))
})

test_that("flow tracks participant flow and exclusions", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  fl <- flow(tab)
  expect_s3_class(fl, "simtab_flow")
  expect_s3_class(fl, "simtab")
})

test_that("codebook produces data dictionary summary of dataset", {
  cb <- codebook(epitabl[, c("age", "sex", "adjudicated_acs")])
  expect_s3_class(cb, "simtab_codebook")
  expect_s3_class(cb, "data.frame")
  expect_true(all(c("variable", "type") %in% names(cb)))
})

test_that("strobe returns STROBE reporting checklist for descriptive result", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  st <- strobe(tab)
  expect_s3_class(st, "simtab_checklist")
})

test_that("stard returns STARD reporting checklist for diagnostic result", {
  dat <- make_diag_data(tp = 36, fp = 8, fn = 4, tn = 52)
  dres <- suppressMessages(diag_test(dat, test = rapid, ref = gold, positive = "Yes"))
  sd <- stard(dres)
  expect_s3_class(sd, "simtab_checklist")
})

test_that("why explains design and analysis choices", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "rr"))
  wh <- why(tab)
  expect_s3_class(wh, "simtab_explanation")
  expect_output(print(wh))
})

test_that("as_methods generates reproducible methods paragraph", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "sex"), by = "adjudicated_acs"))
  meth <- as_methods(tab)
  expect_type(meth, "character")
  expect_true(nzchar(meth))
})


test_that("sensitivity(adjust = NULL) warns when there is no adjustment to drop", {
  fit <- suppressMessages(regtab(
    epitabl, "mace_event", ~ renal_impairment + age,
    family = binomial()
  ))
  expect_warning(sensitivity(fit, adjust = NULL), "left the analysis unchanged")

  # Covariates added with adjust() are dropped, and silently so.
  adjusted <- suppressMessages(adjust(fit, sex))
  expect_no_warning(sens <- sensitivity(adjusted, adjust = NULL))
  rows <- attr(sens, "sensitivity_rows")
  expect_false(isTRUE(all.equal(rows$estimate[[1]], rows$estimate[[2]])))
})

test_that("e_value and sensitivity use the pooled estimate of a stratified tb result", {
  strat <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex))
  pooled <- strat$data$mh[strat$data$mh$row_type == "pooled", ]

  ev <- e_value(strat)
  expect_identical(nrow(ev$data), 1L)
  expect_equal(ev$data$estimate, pooled$estimate)
  expect_match(ev$data$term, "Mantel-Haenszel")

  sens <- suppressMessages(sensitivity(strat, measure = "OR"))
  expect_equal(attr(sens, "sensitivity_rows")$estimate[[1]], pooled$estimate)
})

test_that("tb forest plots draw each ratio once, not once per outcome level", {
  skip_if_not_installed("ggplot2")
  res <- suppressMessages(tb(epitabl, smoking, mace_event, rr, ref = "Never"))
  frame <- SimtablR:::.effect_forest_frame(res)
  expect_identical(nrow(frame), nrow(res$data$ratios))
  expect_length(unique(frame$panel), 1L)
  expect_equal(frame$estimate, res$data$ratios$estimate)

  strat <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex))
  p <- ggplot2::autoplot(strat)
  expect_s3_class(p, "ggplot")
  expect_identical(nrow(p$data), nrow(strat$data$mh))
})

test_that("as_methods names the adjusted model and its covariates", {
  or_adj <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "OR", adjust = c(age, diabetes)))
  meth <- as_methods(or_adj)
  expect_match(meth, "Crude OR was estimated using the Woolf/logit", fixed = TRUE)
  expect_match(meth, "Adjusted OR was estimated by logistic regression", fixed = TRUE)
  expect_match(meth, "Age at index presentation (years) and History of diabetes", fixed = TRUE)

  rr_adj <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "RR", adjust = age))
  expect_match(as_methods(rr_adj), "Adjusted RR was estimated by log-binomial regression adjusting for Age", fixed = TRUE)

  # Adding covariates through the verb updates the prose too.
  crude <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "OR"))
  expect_false(grepl("Adjusted", as_methods(crude)))
  expect_match(as_methods(suppressMessages(adjust(crude, age))), "adjusting for Age", fixed = TRUE)
})
