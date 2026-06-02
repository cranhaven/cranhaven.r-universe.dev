# tests/testthat/test_adiposity_sds.R

library(testthat)
library(tibble)

# Shared reference for tests
ref <- list(
  BMI   = c(mean = 23, sd = 4),
  waist = c(mean = 80, sd = 12)
)

test_that("errors if data missing required columns", {
  skip_on_cran()
  df <- tibble(BMI = 23)
  expect_error(
    adiposity_sds(df, ref = ref),
    "missing required columns: waist"
  )
})

test_that("computes correct SDS for single row", {
  skip_on_cran()
  df <- tibble(BMI = 27, waist = 92)
  out <- adiposity_sds(df, ref, diagnostics = FALSE)  # positional ref (back-compat)
  expect_equal(out$BMI_SDS,  (27 - 23) / 4)
  expect_equal(out$waist_SDS, (92 - 80) / 12)
})

test_that("output columns are named <var>_SDS and returns only SDS columns", {
  skip_on_cran()
  df <- tibble(BMI = c(23, 27), waist = c(80, 92))
  out <- adiposity_sds(df, ref = ref)
  expect_named(out, c("BMI_SDS", "waist_SDS"))
  expect_equal(ncol(out), 2L)
  expect_equal(nrow(out), 2L)
})

test_that("SDS formula is (x - mean) / sd for multiple rows", {
  skip_on_cran()
  df <- tibble(BMI = c(19, 23, 31), waist = c(68, 80, 104))
  out <- adiposity_sds(df, ref = ref)
  expect_equal(out$BMI_SDS,   (df$BMI   - 23) / 4,  tolerance = 1e-10)
  expect_equal(out$waist_SDS, (df$waist - 80) / 12, tolerance = 1e-10)
})

test_that("return_summary = TRUE returns list with data, summary, warnings elements", {
  skip_on_cran()
  df <- tibble(BMI = c(25, NA, 21), waist = c(90, 70, 80))
  res <- adiposity_sds(df, ref = ref, return_summary = TRUE, na_action = "omit")
  expect_type(res, "list")
  expect_named(res, c("data", "summary", "warnings"))
  expect_s3_class(res$data, "tbl_df")
  expect_equal(nrow(res$data), 2L)          # 1 row omitted
  expect_equal(res$summary$omitted_rows, 1L)
  expect_equal(res$summary$rows_in, 3L)
  expect_type(res$warnings, "character")
})

test_that("check_extreme removed: legacy alias warns, function passes through", {
  skip_on_cran()
  df <- tibble(BMI = c(120, 22), waist = c(300, 60))
  # check_raw_extreme (legacy alias) now emits no deprecated warning since param removed
  out <- adiposity_sds(df, ref = ref)
  expect_equal(nrow(out), 2L)
})

test_that("computes SDS on identity mapping", {
  skip_on_cran()
  df <- tibble(BMI = c(23, 27), waist = c(80, 92))
  out <- adiposity_sds(df, ref = ref)
  expect_equal(out$BMI_SDS, c(0, (27-23)/4))
  expect_equal(out$waist_SDS, c(0, (92-80)/12))
})

test_that("na_action policies: keep, omit, error", {
  skip_on_cran()
  df <- tibble(BMI = c(25, NA, 21), waist = c(90, 70, 80))
  expect_equal(nrow(adiposity_sds(df, ref = ref, na_action = "keep", diagnostics = FALSE)), 3)
  expect_equal(nrow(adiposity_sds(df, ref = ref, na_action = "omit", diagnostics = FALSE)), 2)
  expect_error(adiposity_sds(df, ref = ref, na_action = "error", diagnostics = FALSE),
               "rows have missing values")
})

test_that("check_extreme removed: function passes through raw outlier rows", {
  skip_on_cran()
  df <- tibble(BMI = c(120, 22), waist = c(300, 60))
  out <- adiposity_sds(df, ref = ref, diagnostics = FALSE)
  expect_equal(nrow(out), 2L)
})

test_that("SDS extreme handling respects sds_cap and extreme_action", {
  skip_on_cran()
  df <- tibble(BMI = c(23 + 100*4, 23), waist = c(80, 80))
  out_cap <- adiposity_sds(df, ref = ref, extreme_action = "cap", sds_cap = 6)
  expect_lte(max(abs(out_cap$BMI_SDS)), 6)
  expect_error(adiposity_sds(df, ref = ref, extreme_action = "error", sds_cap = 6), "SDS beyond")
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(BMI = 23, waist = 80)
  expect_message(adiposity_sds(df, ref = ref, verbose = TRUE), "adiposity_sds")
  expect_message(adiposity_sds(df, ref = ref, verbose = TRUE), "col_map")
  expect_message(adiposity_sds(df, ref = ref, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(BMI = 23, waist = 80)
  msgs <- testthat::capture_messages(adiposity_sds(df, ref = ref, verbose = TRUE))
  expect_equal(sum(grepl("col_map",  msgs)), 1L)
  expect_equal(sum(grepl("results:",    msgs)), 1L)
})
