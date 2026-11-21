library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("advise returns methodological guidance on simtab_result", {
  tab <- suppressMessages(table1(epitabl, vars = "age", by = "adjudicated_acs"))
  adv <- advise(tab)
  expect_type(adv, "list")
})

test_that("advise rules list registered epidemiological guidance rules", {
  rules <- list_rules()
  expect_s3_class(rules, "data.frame")
  expect_true(nrow(rules) >= 20)
  expect_true(all(c("id", "severity", "citation") %in% names(rules)))
})

test_that("advise triggers model complexity or small sample warnings on regressions", {
  small_df <- data.frame(
    y = factor(c(rep("No", 5), rep("Yes", 5))),
    x1 = rnorm(10),
    x2 = rnorm(10),
    x3 = rnorm(10)
  )
  fit <- suppressMessages(suppressWarnings(regtab(small_df, outcomes = "y", predictors = ~ x1 + x2 + x3, family = stats::binomial("logit"))))
  aud <- advise(fit, audit = TRUE)
  df <- as.data.frame(aud)
  expect_true(any(df$fired))
})

test_that("advise identifies multiple unadjusted comparisons", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "bmi", "cholesterol"), by = "adjudicated_acs", test = TRUE))
  aud <- advise(tab, audit = TRUE)
  df <- as.data.frame(aud)
  expect_true("multiplicity_unadjusted" %in% df$id)
})

test_that("advise with audit = TRUE returns complete rule audit data frame", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  aud <- advise(tab, audit = TRUE)
  expect_s3_class(aud, "simtab_audit")
  expect_s3_class(as.data.frame(aud), "data.frame")
  expect_true(all(c("id", "severity", "fired") %in% names(as.data.frame(aud))))
})

test_that("advise rules remain non-blocking and do not mutate result evidence", {
  tab <- suppressMessages(table1(epitabl, "age", by = "adjudicated_acs"))
  raw_before <- tab$data
  adv <- advise(tab)
  expect_identical(tab$data, raw_before)
})

test_that("advise flags a risk ratio reported under a case-control design", {
  rr_cc <- suppressMessages(tb(epitabl, renal_impairment, mace_event, measure = "RR", design = "case_control"))
  expect_identical(unique(rr_cc$data$ratios$type), "RR")
  audit <- as.data.frame(advise(rr_cc, audit = TRUE))
  expect_true(audit$fired[audit$id == "ratio_in_case_control"])

  # The design default (OR) and other designs stay silent; advice never blocks.
  or_cc <- suppressMessages(tb(epitabl, renal_impairment, mace_event, design = "case_control"))
  expect_false("ratio_in_case_control" %in% as.data.frame(advise(or_cc, audit = TRUE))$id[
    as.data.frame(advise(or_cc, audit = TRUE))$fired
  ])
  cohort <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "RR", design = "cohort"))
  cohort_audit <- as.data.frame(advise(cohort, audit = TRUE))
  expect_false(any(cohort_audit$fired[cohort_audit$id == "ratio_in_case_control"]))

  t1_cc <- suppressMessages(table1(epitabl, sex, by = mace_event, measure = "RR", design = "case_control"))
  t1_audit <- as.data.frame(advise(t1_cc, audit = TRUE))
  expect_true(t1_audit$fired[t1_audit$id == "ratio_in_case_control"])
})
