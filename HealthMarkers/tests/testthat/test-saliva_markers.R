# tests/testthat/test_saliva_markers.R

library(testthat)
library(tibble)

test_that("saliva_markers errors if missing required columns", {
  skip_on_cran()
  df1 <- tibble(
    saliva_cort2    = 15,
    saliva_cort3    = 12,
    saliva_amylase  = 100,
    saliva_glucose  = 80
  )
  expect_error(
    saliva_markers(df1),
    "cort1"
  )

  df2 <- tibble(
    saliva_cort1    = 10,
    saliva_cort3    = 12,
    saliva_amylase  = 100
    # missing saliva_cort2 and saliva_glucose
  )
  expect_error(
    saliva_markers(df2),
    "cort2"
  )
})

test_that("saliva_markers computes log transforms, CAR_AUC, and passes through glucose", {
  skip_on_cran()
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  out <- saliva_markers(df)

  expect_named(
    out,
    c("log_cortisol_wake", "CAR_AUC", "log_amylase", "saliva_glucose")
  )
  expect_equal(nrow(out), 1)

  expected_log_cort <- log(10)
  # trapezoidal AUC: (10+20)/2*30 + (20+10)/2*30 = 900
  expected_auc <- (10 + 20) / 2 * 30 + (20 + 10) / 2 * 30
  expected_log_amy <- log(100)
  expected_gluc <- 5.0

  expect_equal(out$log_cortisol_wake, expected_log_cort)
  expect_equal(out$CAR_AUC, expected_auc)
  expect_equal(out$log_amylase, expected_log_amy)
  expect_equal(out$saliva_glucose, expected_gluc)
})

test_that("saliva_markers is vectorized over multiple rows", {
  skip_on_cran()
  df <- tibble(
    saliva_cort1    = c(10, 5),
    saliva_cort2    = c(20, 10),
    saliva_cort3    = c(10, 5),
    saliva_amylase  = c(100, 50),
    saliva_glucose  = c(5, 3)
  )
  out <- saliva_markers(df)
  expect_equal(nrow(out), 2)
  expect_equal(out$log_cortisol_wake[2], log(5))
  # AUC second row: (5+10)/2*30 + (10+5)/2*30 = 450
  expect_equal(out$CAR_AUC[2], 450)
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(saliva_markers(df, verbose = TRUE), "saliva_markers")
  expect_message(saliva_markers(df, verbose = TRUE), "col_map")
  expect_message(saliva_markers(df, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  withr::local_options(healthmarkers.verbose = "inform")
  msgs <- testthat::capture_messages(saliva_markers(df, verbose = TRUE))
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("na_action='omit' drops rows with NA in required inputs and returns empty tibble when all omitted", {
  skip_on_cran()
  df <- tibble(
    saliva_cort1    = NA_real_,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  out <- saliva_markers(df, na_action = "omit")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("log_cortisol_wake","CAR_AUC","log_amylase","saliva_glucose"))
})

test_that("invalid `times` argument errors clearly", {
  skip_on_cran()
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100,
    saliva_glucose  = 5.0
  )
  expect_error(
    saliva_markers(df, times = c(0, 60)),
    "times"
  )
})

test_that("extreme values produce range note in verbose; no warning in non-verbose mode", {
  skip_on_cran()
  df <- tibble(
    saliva_cort1    = 10,
    saliva_cort2    = 20,
    saliva_cort3    = 10,
    saliva_amylase  = 100000,  # above plausible range of 50000
    saliva_glucose  = 5.0
  )
  # No warning emitted in non-verbose mode
  expect_no_warning(saliva_markers(df, verbose = FALSE))

  # Values are not altered; log_amylase uses original extreme value
  out <- saliva_markers(df, verbose = FALSE)
  expect_equal(out$log_amylase, log(100000), tolerance = 1e-12)

  # Verbose mode emits a range note informational message
  withr::local_options(healthmarkers.verbose = "inform")
  expect_message(saliva_markers(df, verbose = TRUE), "range note")
})
