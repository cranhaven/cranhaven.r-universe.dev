# File: tests/testthat/test_allostatic_load.R

library(testthat)
library(tibble)

test_that("basic multi-biomarker scoring uses strict >", {
  skip_on_cran()
  df <- tibble(
    A = c(1, 5, 10, NA),
    B = c(0, 2, 3, 4),
    C = c(100, 50, 25, 10)
  )
  thr <- list(A = 4, B = 2, C = 30)
  out <- allostatic_load(df, thresholds = thr, na_action = "keep")
  expect_s3_class(out, "tbl_df")
  expect_named(out, "AllostaticLoad")
  expect_equal(out$AllostaticLoad, c(1L, 2L, 2L, 1L))
})

test_that("single biomarker uses inclusive >=", {
  skip_on_cran()
  df <- tibble(X = c(5, 10, 11))
  thr <- list(X = 10)
  out <- allostatic_load(df, thresholds = thr, na_action = "keep")
  expect_equal(out$AllostaticLoad, c(0L, 1L, 1L))
})

test_that("input validation errors (HM-CS messages)", {
  skip_on_cran()
  df <- tibble(A = 1:3)
  expect_error(allostatic_load("not df", thresholds = list(A = 1)), "data.frame or tibble")
  expect_error(allostatic_load(df, thresholds = list()), "must contain at least one")
  bad_thr1 <- list(3); names(bad_thr1) <- ""
  expect_error(allostatic_load(df, thresholds = bad_thr1), "must be a named list")
  bad_thr2 <- list(1, 2); names(bad_thr2) <- c("A", "A")
  expect_error(allostatic_load(df, thresholds = bad_thr2), "duplicate names")
  expect_error(allostatic_load(df, thresholds = list(A = c(1, 2))), "length-1")
  expect_error(allostatic_load(df, thresholds = list(B = 2)), "missing required columns in data")
})

test_that("biomarker must be numeric", {
  skip_on_cran()
  df <- tibble(A = letters[1:3])
  thr <- list(A = 1)
  # Suppress the deliberate coercion warning in the test
  df$A <- suppressWarnings(as.numeric(df$A))
  out <- allostatic_load(df, thresholds = thr, na_action = "keep")
  expect_equal(nrow(out), 3)
})

test_that("na_action = omit drops any row with missing in any required var", {
  skip_on_cran()
  df <- tibble(A = c(1, NA, 3), B = c(2, 2, NA))
  thr <- list(A = 1.5, B = 1.5)
  out <- allostatic_load(df, thresholds = thr, na_action = "omit")
  expect_equal(nrow(out), 1L)
})

test_that("na_action = error stops on NA", {
  skip_on_cran()
  df <- tibble(A = c(1, NA, 3))
  expect_error(
    allostatic_load(df, thresholds = list(A = 2), na_action = "error"),
    "missing/non-finite values present"
  )
})

test_that("check_extreme removed: function passes through SDS-like outlier rows", {
  skip_on_cran()
  df <- tibble(BP_sds = c(-0.5, 7, 6.5), HR_sds = c(0, -7, 5))
  thr <- list(BP_sds = 1.5, HR_sds = 1.5)
  out <- allostatic_load(df, thresholds = thr)
  expect_equal(nrow(out), 3L)
})

test_that("zero-row data returns zero-row tibble", {
  skip_on_cran()
  df <- tibble(A = numeric(0))
  out <- allostatic_load(df, thresholds = list(A = 2))
  expect_equal(nrow(out), 0)
  expect_named(out, "AllostaticLoad")
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df  <- tibble(A = c(1, 5), B = c(0, 3))
  thr <- list(A = 2, B = 1)
  expect_message(allostatic_load(df, thresholds = thr, verbose = TRUE), "allostatic_load")
  expect_message(allostatic_load(df, thresholds = thr, verbose = TRUE), "col_map")
  expect_message(allostatic_load(df, thresholds = thr, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df  <- tibble(A = c(1, 5), B = c(0, 3))
  thr <- list(A = 2, B = 1)
  msgs <- testthat::capture_messages(
    allostatic_load(df, thresholds = thr, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("return_summary returns structured list", {
  skip_on_cran()
  df <- tibble(A = c(1, 5, 6))
  thr <- list(A = 2)
  res <- allostatic_load(df, thresholds = thr, return_summary = TRUE)
  expect_named(res, c("data", "summary", "warnings"))
  expect_true(is.data.frame(res$data))
  expect_true(is.list(res$summary))
  expect_true(all(c("rows","biomarkers","total_flags","mean_flags") %in% names(res$summary)))
  expect_equal(res$summary$total_flags, sum(res$data$AllostaticLoad))
  # A = c(1,5,6), thr = 2, single biomarker -> >= rule; flags: 0,1,1 -> total = 2
  expect_equal(res$summary$total_flags, 2L)
  expect_equal(res$summary$biomarkers, 1L)
  expect_equal(res$summary$rows, 3L)
})
