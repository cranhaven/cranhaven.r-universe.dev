library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("diag_test computes 2x2 diagnostic metrics (sens, spec, PPV, NPV, LR)", {
  dat <- make_diag_data(tp = 36, fp = 8, fn = 4, tn = 52)
  result <- suppressMessages(diag_test(dat, test = rapid, ref = gold, positive = "Yes"))
  expect_s3_class(result, "diag_test")
  expect_s3_class(result, "simtab_result")
  expect_true(all(c("sensitivity", "specificity", "ppv", "npv", "lr_pos", "lr_neg") %in% rownames(result$data$metrics)))
  expect_equal(unname(result$data$confusion_matrix), matrix(c(36, 4, 8, 52), nrow = 2))
})

test_that("diag_test computes confidence intervals", {
  dat <- make_diag_data(tp = 36, fp = 8, fn = 4, tn = 52)
  result <- suppressMessages(diag_test(dat, test = rapid, ref = gold, positive = "Yes"))
  expect_true("conf.low" %in% colnames(result$data$metrics))
  expect_true("conf.high" %in% colnames(result$data$metrics))
  expect_true(!is.na(result$data$metrics["sensitivity", "conf.low"]))
  expect_true(!is.na(result$data$metrics["specificity", "conf.high"]))
})

test_that("diag_test supports exact and wilson CI methods", {
  dat <- make_diag_data(tp = 36, fp = 8, fn = 4, tn = 52)
  res_exact <- suppressMessages(diag_test(dat, test = rapid, ref = gold, positive = "Yes", ci = "exact"))
  res_wilson <- suppressMessages(diag_test(dat, test = rapid, ref = gold, positive = "Yes", ci = "wilson"))
  expect_identical(res_exact$meta$ci, "exact")
  expect_identical(res_wilson$meta$ci, "wilson")
})

test_that("diag_test print and as.data.frame accessors work", {
  dat <- make_diag_data(tp = 36, fp = 8, fn = 4, tn = 52)
  result <- suppressMessages(diag_test(dat, test = rapid, ref = gold, positive = "Yes"))
  df <- as.data.frame(result)
  expect_s3_class(df, "data.frame")
  expect_output(print(result))
})

test_that("roc computes ROC curve and AUC", {
  skip_if_not_installed("pROC")
  fit <- suppressMessages(roc(epitabl, marker = age, outcome = adjudicated_acs, positive = "Yes"))
  expect_s3_class(fit, "simtab_roc")
  expect_s3_class(fit, "simtab_result")
  expect_true(is.numeric(fit$data$auc$auc))
  expect_true(fit$data$auc$auc >= 0 && fit$data$auc$auc <= 1)
})

test_that("roc computes optimal cutpoint via Youden's J", {
  skip_if_not_installed("pROC")
  fit <- suppressMessages(roc(epitabl, marker = age, outcome = adjudicated_acs, positive = "Yes"))
  expect_true(!is.null(fit$data$cutpoint))
  expect_true(is.numeric(fit$data$cutpoint$threshold))
})

test_that("roc computes DeLong confidence intervals for AUC", {
  skip_if_not_installed("pROC")
  fit <- suppressMessages(roc(epitabl, marker = age, outcome = adjudicated_acs, positive = "Yes"))
  expect_true(!is.null(fit$data$auc$conf.low))
  expect_true(!is.null(fit$data$auc$conf.high))
  expect_true(fit$data$auc$conf.low <= fit$data$auc$auc)
})

test_that("roc and diag_test validate inputs", {
  dat <- make_diag_data(tp = 36, fp = 8, fn = 4, tn = 52)
  expect_error(diag_test(dat, test = nonexistent, ref = gold))
  expect_error(roc(epitabl, marker = nonexistent, outcome = adjudicated_acs))
})


test_that("a degenerate kappa interval renders without a negative zero", {
  dat <- data.frame(
    rapid = factor(rep("Positive", 50), levels = c("Negative", "Positive")),
    gold = factor(c(rep("Yes", 30), rep("No", 20)), levels = c("No", "Yes"))
  )
  res <- suppressWarnings(suppressMessages(diag_test(dat, test = rapid, ref = gold)))
  expect_lt(res$data$metrics["kappa", "conf.low"], 0)

  display <- as.data.frame(res)
  kappa_ci <- display$CI[display$Metric == "Cohen's Kappa"]
  expect_false(grepl("-0.00", kappa_ci, fixed = TRUE))
  expect_output(print(res), "Cohen's Kappa\\s+:\\s+0\\.00\\s+\\(0\\.00 - 0\\.00\\)")
  # Presentation-only: the raw interval is untouched.
  expect_identical(fmt(res, d = 3)$data, res$data)

  # Real negative values keep their sign.
  expect_identical(SimtablR:::.unsign_zero(c("-0.00", "-0.01", "-0,0")), c("0.00", "-0.01", "0,0"))
})

test_that("label() relabels ROC markers and the outcome without touching evidence", {
  skip_if_not_installed("pROC")
  res <- suppressMessages(roc(epitabl, c(poc_hstn_value, age), adjudicated_acs))
  relabelled <- label(res, poc_hstn_value = "POC troponin", adjudicated_acs = "Adjudicated ACS")

  expect_identical(relabelled$data, res$data)
  expect_identical(as.data.frame(relabelled)$Marker, c("POC troponin", "age"))
  expect_identical(as.data.frame(res)$Marker, c("poc_hstn_value", "age"))
  out <- capture.output(print(relabelled))
  expect_true(any(grepl("Outcome: Adjudicated ACS", out, fixed = TRUE)))
  expect_true(any(grepl("POC troponin vs age", out, fixed = TRUE)))
})
