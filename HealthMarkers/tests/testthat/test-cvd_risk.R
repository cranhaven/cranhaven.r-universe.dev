# tests/testthat/test_cvd_risk.R

library(testthat)
library(HealthMarkers)

# Reusable dummy data (minimal columns)
dummy_df <- tibble::tibble(
  age        = 55,
  sex        = 1,
  race       = "white",
  total_chol = 200,
  HDL_c      = 50,
  TG         = 150,
  sbp        = 140,
  bp_treated = TRUE,
  smoker     = FALSE,
  diabetes   = FALSE,
  bmi        = 27
)

test_that("dummy_df is a tibble", {
  skip_on_cran()
  expect_s3_class(dummy_df, "tbl_df")
})

# ---- AIP (no optional deps) ----
test_that("cvd_risk AIP returns log10(TG/HDL_c)", {
  skip_on_cran()
  out <- cvd_risk(dummy_df, model = "AIP", col_map = list(TG = "TG", HDL_c = "HDL_c"))
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "AIP")
  expect_equal(out$value, log10(150 / 50))
})

# ---- LDL_PN (no optional deps) ----
test_that("cvd_risk LDL_PN returns ApoB value", {
  skip_on_cran()
  df2 <- tibble::tibble(ApoB = 120)
  out2 <- cvd_risk(df2, model = "LDL_PN", col_map = list(ApoB = "ApoB"))
  expect_s3_class(out2, "tbl_df")
  expect_identical(out2$model, "LDL_PN")
  expect_identical(out2$value, 120)
})

# ---- ASCVD (PooledCohort) ----
test_that("cvd_risk ASCVD returns tibble with numeric risk when PooledCohort present", {
  skip_on_cran()
  skip_if_not_installed("PooledCohort")
  out <- cvd_risk(dummy_df, model = "ASCVD", year = 10)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "ASCVD")
  expect_identical(out$year, 10L)            # integer now
  expect_type(out$risk, "double")
  expect_equal(length(out$risk), nrow(dummy_df))
})

# ---- Stroke (PooledCohort, PCE equations) ----
test_that("cvd_risk Stroke returns tibble with numeric risk when PooledCohort present", {
  skip_on_cran()
  skip_if_not_installed("PooledCohort")
  out <- cvd_risk(dummy_df, model = "Stroke")
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "Stroke")
  expect_identical(out$year, 10L)            # integer now
  expect_type(out$risk, "double")
  expect_equal(length(out$risk), nrow(dummy_df))
})

# ---- QRISK3 (many inputs; handle both NA fallback and real result) ----
test_that("cvd_risk QRISK3 returns tibble when QRISK3 present (NA fallback or numeric)", {
  skip_on_cran()
  skip_if_not_installed("QRISK3")
  out <- cvd_risk(dummy_df, model = "QRISK3")
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "QRISK3")
  expect_identical(out$year, 10L)
  if (all(is.na(out$risk))) {
    expect_equal(nrow(out), 1L)  # backend error path -> placeholder
  } else {
    expect_type(out$risk, "double")
    expect_equal(length(out$risk), nrow(dummy_df))
  }
})

# ---- RiskScorescvd passthrough ----
# Skip if package not installed; structure depends on upstream.
test_that("cvd_risk RiskScorescvd returns a data frame when package present", {
  skip_on_cran()
  skip_if_not_installed("RiskScorescvd")
  out <- cvd_risk(dummy_df, model = "RiskScorescvd")
  expect_true(is.data.frame(out))
  expect_true("model" %in% names(out))
})

# ---- Invalid model ----
test_that("cvd_risk errors on invalid model", {
  skip_on_cran()
  expect_error(cvd_risk(dummy_df, model = "INVALID"))
})

# ---- Dispatcher: ALL ----
test_that("cvd_risk ALL returns one row per model with expected columns", {
  skip_on_cran()
  out_all <- cvd_risk(dummy_df, model = "ALL", year = 10)
  expect_s3_class(out_all, "tbl_df")
  expect_true(all(c("model", "year", "risk", "value") %in% names(out_all)))
  expect_setequal(
    out_all$model,
    c("ASCVD", "QRISK3", "Stroke", "RiskScorescvd", "AIP", "LDL_PN")
  )
  expect_equal(nrow(out_all), 6L)
})

# ---- Direct: cvd_marker_aip ------------------------------------------------
test_that("cvd_marker_aip returns tibble with model=AIP and numeric value", {
  skip_on_cran()
  df_aip <- tibble::tibble(TG = 150, HDL_c = 50)
  out <- cvd_marker_aip(df_aip)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "AIP")
  expect_equal(out$value, log10(150 / 50))
})

test_that("cvd_marker_aip partial col_map is supplemented by inference", {
  skip_on_cran()
  df_aip <- tibble::tibble(TG = 150, HDL_c = 50)
  expect_no_error(cvd_marker_aip(df_aip, col_map = list(TG = "TG")))
})

test_that("cvd_marker_aip returns NA value when inputs missing (na_action='keep')", {
  skip_on_cran()
  df_na <- tibble::tibble(TG = NA_real_, HDL_c = 50)
  out <- cvd_marker_aip(df_na, na_action = "keep")
  expect_true(is.na(out$value))
})

test_that("cvd_marker_aip verbose = TRUE emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df_aip <- tibble::tibble(TG = 150, HDL_c = 50)
  cm <- list(TG = "TG", HDL_c = "HDL_c")
  expect_message(cvd_marker_aip(df_aip, col_map = cm, verbose = TRUE), "cvd_marker_aip")
  expect_message(cvd_marker_aip(df_aip, col_map = cm, verbose = TRUE), "col_map")
  expect_message(cvd_marker_aip(df_aip, col_map = cm, verbose = TRUE), "results:")
})

test_that("cvd_marker_aip verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df_aip <- tibble::tibble(TG = 150, HDL_c = 50)
  cm   <- list(TG = "TG", HDL_c = "HDL_c")
  msgs <- testthat::capture_messages(cvd_marker_aip(df_aip, col_map = cm, verbose = TRUE))
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

# ---- Direct: cvd_marker_ldl_particle_number --------------------------------
test_that("cvd_marker_ldl_particle_number returns tibble with model=LDL_PN", {
  skip_on_cran()
  df_ldl <- tibble::tibble(ApoB = 120)
  out <- cvd_marker_ldl_particle_number(df_ldl)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "LDL_PN")
  expect_identical(out$value, 120)
})

test_that("cvd_marker_ldl_particle_number returns NA when ApoB column absent and col_map=NULL", {
  skip_on_cran()
  df_ldl <- tibble::tibble(x = 1)
  out <- cvd_marker_ldl_particle_number(df_ldl)
  expect_s3_class(out, "tbl_df")
  expect_true(is.na(out$value))
})

test_that("cvd_marker_ldl_particle_number errors on missing ApoB when col_map explicit", {
  skip_on_cran()
  df_ldl <- tibble::tibble(x = 1)
  expect_error(cvd_marker_ldl_particle_number(df_ldl, col_map = list(ApoB = "ApoB")))
})

test_that("cvd_marker_ldl_particle_number verbose = TRUE emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df_ldl <- tibble::tibble(ApoB = 120)
  cm <- list(ApoB = "ApoB")
  expect_message(cvd_marker_ldl_particle_number(df_ldl, col_map = cm, verbose = TRUE), "cvd_marker_ldl_particle_number")
  expect_message(cvd_marker_ldl_particle_number(df_ldl, col_map = cm, verbose = TRUE), "col_map")
  expect_message(cvd_marker_ldl_particle_number(df_ldl, col_map = cm, verbose = TRUE), "results:")
})

test_that("cvd_marker_ldl_particle_number verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df_ldl <- tibble::tibble(ApoB = 120)
  cm   <- list(ApoB = "ApoB")
  msgs <- testthat::capture_messages(cvd_marker_ldl_particle_number(df_ldl, col_map = cm, verbose = TRUE))
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

# ---- Direct: cvd_risk_ascvd ------------------------------------------------
test_that("cvd_risk_ascvd returns tibble with model, year, risk (PooledCohort)", {
  skip_on_cran()
  skip_if_not_installed("PooledCohort")
  out <- cvd_risk_ascvd(dummy_df, year = 10)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "ASCVD")
  expect_identical(out$year, 10L)
  expect_type(out$risk, "double")
})

test_that("cvd_risk_ascvd errors on invalid year", {
  skip_on_cran()
  skip_if_not_installed("PooledCohort")
  expect_error(cvd_risk_ascvd(dummy_df, year = 5))
})

test_that("cvd_risk_ascvd errors if PooledCohort missing", {
  skip_on_cran()
  skip_if(requireNamespace("PooledCohort", quietly = TRUE))
  expect_error(cvd_risk_ascvd(dummy_df))
})

# ---- Direct: cvd_risk_stroke -----------------------------------------------
test_that("cvd_risk_stroke returns tibble with model=Stroke (PooledCohort)", {
  skip_on_cran()
  skip_if_not_installed("PooledCohort")
  out <- cvd_risk_stroke(dummy_df)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "Stroke")
  expect_identical(out$year, 10L)
  expect_type(out$risk, "double")
})

test_that("cvd_risk_stroke errors on non-data.frame input", {
  skip_on_cran()
  skip_if_not_installed("PooledCohort")
  expect_error(cvd_risk_stroke("not a df"))
})

# ---- Direct: cvd_risk_qrisk3 -----------------------------------------------
test_that("cvd_risk_qrisk3 returns tibble with model=QRISK3 (QRISK3 pkg)", {
  skip_on_cran()
  skip_if_not_installed("QRISK3")
  out <- cvd_risk_qrisk3(dummy_df)
  expect_s3_class(out, "tbl_df")
  expect_identical(out$model, "QRISK3")
})

test_that("cvd_risk_qrisk3 errors if QRISK3 package missing", {
  skip_on_cran()
  skip_if(requireNamespace("QRISK3", quietly = TRUE))
  expect_error(cvd_risk_qrisk3(dummy_df))
})

# ---- Direct: cvd_risk_scorescvd --------------------------------------------
test_that("cvd_risk_scorescvd returns a data frame when RiskScorescvd present", {
  skip_on_cran()
  skip_if_not_installed("RiskScorescvd")
  out <- cvd_risk_scorescvd(dummy_df)
  expect_true(is.data.frame(out))
})

test_that("cvd_risk_scorescvd errors if RiskScorescvd package missing", {
  skip_on_cran()
  skip_if(requireNamespace("RiskScorescvd", quietly = TRUE))
  expect_error(cvd_risk_scorescvd(dummy_df))
})
