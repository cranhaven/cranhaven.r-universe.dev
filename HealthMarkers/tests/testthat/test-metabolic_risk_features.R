library(testthat)
library(tibble)
library(HealthMarkers)

test_that("returns tibble with four factor flags and correct names/levels", {
  skip_on_cran()
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = 5.8, HbA1c = 40,
    bp_sys_z = 1.7, bp_dia_z = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- metabolic_risk_features(df, col_map = cm)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("dyslipidemia", "insulin_resistance", "hyperglycemia", "hypertension"))
  lapply(out, function(x) {
    expect_true(is.factor(x))
    expect_identical(levels(x), c("0", "1"))
  })

  # All flags should be "1"
  vals <- vapply(out, function(x) as.integer(as.character(x)), integer(1))
  expect_equal(unname(vals), c(1L, 1L, 1L, 1L))
})

test_that("custom col_map works", {
  skip_on_cran()
  df <- tibble(
    CT = 6.0, LDL = 3.5, HDL = 1.0, TG = 1.2,
    AGE = 25, ZH = 1.5, GLU = 5.8, A1C = 40, SBPZ = 1.7, DBPZ = 1.0
  )
  cm <- list(
    chol_total = "CT", chol_ldl = "LDL", chol_hdl = "HDL", triglycerides = "TG",
    age_year = "AGE", z_HOMA = "ZH", glucose = "GLU", HbA1c = "A1C",
    bp_sys_z = "SBPZ", bp_dia_z = "DBPZ"
  )
  out <- metabolic_risk_features(df, col_map = cm)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("dyslipidemia", "insulin_resistance", "hyperglycemia", "hypertension"))
})

test_that("errors if mapped columns are missing in data", {
  skip_on_cran()
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = 5.8,
    bp_sys_z = 1.7, bp_dia_z = 1.0
    # HbA1c missing
  )
  cm <- list(
    chol_total = "chol_total", chol_ldl = "chol_ldl", chol_hdl = "chol_hdl", triglycerides = "triglycerides",
    age_year = "age_year", z_HOMA = "z_HOMA", glucose = "glucose", HbA1c = "HbA1c",
    bp_sys_z = "bp_sys_z", bp_dia_z = "bp_dia_z"
  )
  expect_error(
    metabolic_risk_features(df, col_map = cm),
    "mapped columns not found in data"
  )
})

test_that("partial col_map is supplemented by dictionary inference for missing keys", {
  skip_on_cran()
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = 5.8, HbA1c = 40, bp_sys_z = 1.7, bp_dia_z = 1.0
  )
  cm <- list(
    chol_total = "chol_total", chol_ldl = "chol_ldl", chol_hdl = "chol_hdl", triglycerides = "triglycerides",
    age_year = "age_year", z_HOMA = "z_HOMA", glucose = "glucose"
    # HbA1c, bp_sys_z, bp_dia_z missing -> should be inferred from data
  )
  expect_no_error(metabolic_risk_features(df, col_map = cm))
  out <- metabolic_risk_features(df, col_map = cm)
  expect_s3_class(out, "tbl_df")
})

test_that("na_action='error' aborts when required inputs contain NA", {
  skip_on_cran()
  df <- tibble(
    chol_total = 6.0, chol_ldl = 3.5, chol_hdl = 1.0, triglycerides = 1.2,
    age_year = 25, z_HOMA = 1.5, glucose = NA_real_, HbA1c = 40, bp_sys_z = 1.7, bp_dia_z = 1.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_error(
    suppressWarnings(metabolic_risk_features(df, col_map = cm, na_action = "error")),
    "required inputs contain missing values"
  )
})

test_that("na_action='omit' drops rows with NA", {
  skip_on_cran()
  df <- tibble(
    chol_total = c(6.0, 6.0), chol_ldl = c(3.5, 3.5), chol_hdl = c(1.0, 1.0), triglycerides = c(1.2, 1.2),
    age_year = c(25, 25), z_HOMA = c(1.5, 1.5), glucose = c(5.8, NA_real_), HbA1c = c(40, 40),
    bp_sys_z = c(1.7, 1.7), bp_dia_z = c(1.0, 1.0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(metabolic_risk_features(df, col_map = cm, na_action = "omit"))
  expect_equal(nrow(out), 1L)
})

test_that("na_action='keep' yields NA only when the rule cannot be decided", {
  skip_on_cran()
  # Make dyslipidemia undecidable due to NA (no other trigger is TRUE)
  df <- tibble(
    chol_total = 5.0, chol_ldl = NA_real_, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 25, z_HOMA = 0.0, glucose = 5.0, HbA1c = 40, bp_sys_z = 0.0, bp_dia_z = 0.0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- suppressWarnings(metabolic_risk_features(df, col_map = cm, na_action = "keep"))
  expect_true(is.na(out$dyslipidemia))
})

test_that("extreme input values pass through without error", {
  skip_on_cran()
  df <- tibble(
    chol_total = 5.0, chol_ldl = 3.0, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 300,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- metabolic_risk_features(df, col_map = cm)
  expect_s3_class(out, "tbl_df")
})

test_that("high missingness warning is emitted when proportion exceeds threshold", {
  skip_on_cran()
  df <- tibble(
    chol_total = c(5, 5), chol_ldl = c(3, 3), chol_hdl = c(1.2, 1.2), triglycerides = c(1, 1),
    age_year = c(30, 30), z_HOMA = c(0, 0), glucose = c(5, 5),
    HbA1c = c(NA_real_, 40), bp_sys_z = c(0, 0), bp_dia_z = c(0, 0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    metabolic_risk_features(df, col_map = cm, na_action = "warn", na_warn_prop = 0.2),
    "has high missingness"
  )
})

test_that("verbose emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    chol_total = 5, chol_ldl = 3, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 40,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_message(metabolic_risk_features(df, col_map = cm, verbose = TRUE), "metabolic_risk_features")
  expect_message(metabolic_risk_features(df, col_map = cm, verbose = TRUE), "col_map")
  expect_message(metabolic_risk_features(df, col_map = cm, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    chol_total = 5, chol_ldl = 3, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 40,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  msgs <- testthat::capture_messages(
    metabolic_risk_features(df, col_map = cm, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("output has correct row count without check_extreme", {
  skip_on_cran()
  df <- tibble(
    chol_total = 5.0, chol_ldl = 3.0, chol_hdl = 1.2, triglycerides = 1.0,
    age_year = 30, z_HOMA = 0, glucose = 5.0, HbA1c = 300,
    bp_sys_z = 0, bp_dia_z = 0
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  out <- metabolic_risk_features(df, col_map = cm)
  expect_equal(nrow(out), 1L)
})

test_that("numeric coercion warns when NAs introduced", {
  skip_on_cran()
  df <- tibble(
    chol_total = c("5.0","oops"), chol_ldl = c(3.0, 3.0), chol_hdl = c(1.2, 1.2), triglycerides = c(1.0, 1.0),
    age_year = c(30, 30), z_HOMA = c(0, 0), glucose = c(5.0, 5.0), HbA1c = c(40, 40),
    bp_sys_z = c(0, 0), bp_dia_z = c(0, 0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  expect_warning(
    metabolic_risk_features(df, col_map = cm),
    "coerced to numeric; NAs introduced"
  )
})

test_that("na_action='omit' drops rows with required NA and announces omission", {
  skip_on_cran()
  df <- tibble(
    chol_total = c(5.0, NA), chol_ldl = c(3.0, 3.0), chol_hdl = c(1.2, 1.2), triglycerides = c(1.0, 1.0),
    age_year = c(30, 30), z_HOMA = c(0, 0), glucose = c(5.0, 5.0), HbA1c = c(40, 40),
    bp_sys_z = c(0, 0), bp_dia_z = c(0, 0)
  )
  cm <- as.list(names(df)); names(cm) <- names(df)
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(
    out <- metabolic_risk_features(df, col_map = cm, na_action = "omit", verbose = TRUE),
    "omitting 1 rows with NA in required inputs"
  )
  expect_equal(nrow(out), 1L)
})
