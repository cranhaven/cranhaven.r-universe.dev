library(testthat)
library(tibble)
library(HealthMarkers)

test_that("errors when no ratio can be computed from supplied col_map", {
  skip_on_cran()
  df <- tibble(x = 1)
  # Only one key provided; no ratio has both its inputs mapped
  expect_error(
    hormone_markers(df, col_map = list(total_testosterone = "x")),
    "no computable ratios"
  )
})

test_that("computes all eleven ratios correctly", {
  skip_on_cran()
  df <- tibble(
    total_testosterone = 10, SHBG = 2,
    LH = 8, FSH = 4,
    estradiol = 100, progesterone = 50,
    TSH = 2, free_T3 = 5, free_T4 = 10,
    aldosterone = 20, renin = 4,
    insulin = 20, glucagon = 10,
    GH = 2, IGF1 = 4,
    prolactin = 12,
    cortisol_0 = 200, cortisol_30 = 260
  )
  cm <- setNames(as.list(names(df)), names(df))
  out <- hormone_markers(df, col_map = cm, na_action = "keep")
  expect_named(out, c(
    "FAI", "LH_FSH", "E2_P", "E2_T", "T3_T4", "TSH_fT4",
    "ARR", "Ins_Glu", "GH_IGF1", "PRL_T", "CAR_slope"
  ))
  expect_equal(out$FAI,       (10 / 2) * 100)
  expect_equal(out$LH_FSH,    8 / 4)
  expect_equal(out$E2_P,      100 / 50)
  expect_equal(out$E2_T,      100 / 10)
  expect_equal(out$T3_T4,     5 / 10)
  expect_equal(out$TSH_fT4,   2 / 10)
  expect_equal(out$ARR,       20 / 4)
  expect_equal(out$Ins_Glu,   20 / 10)
  expect_equal(out$GH_IGF1,   2 / 4)
  expect_equal(out$PRL_T,     12 / 10)
  expect_equal(out$CAR_slope, (260 - 200) / 30)
})

test_that("verbose emits preparing, column map, and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    total_testosterone = 10, SHBG = 2, LH = 8, FSH = 4,
    estradiol = 100, progesterone = 50, TSH = 2, free_T3 = 5, free_T4 = 10,
    aldosterone = 20, renin = 4, insulin = 20, glucagon = 10,
    GH = 2, IGF1 = 4, prolactin = 12, cortisol_0 = 200, cortisol_30 = 260
  )
  cm <- setNames(as.list(names(df)), names(df))
  expect_message(hormone_markers(df, col_map = cm, verbose = TRUE, na_action = "keep"), "hormone_markers")
  expect_message(hormone_markers(df, col_map = cm, verbose = TRUE, na_action = "keep"), "col_map")
  expect_message(hormone_markers(df, col_map = cm, verbose = TRUE, na_action = "keep"), "results:")
})

test_that("verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    total_testosterone = 10, SHBG = 2, LH = 8, FSH = 4,
    estradiol = 100, progesterone = 50, TSH = 2, free_T3 = 5, free_T4 = 10,
    aldosterone = 20, renin = 4, insulin = 20, glucagon = 10,
    GH = 2, IGF1 = 4, prolactin = 12, cortisol_0 = 200, cortisol_30 = 260
  )
  cm <- setNames(as.list(names(df)), names(df))
  msgs <- testthat::capture_messages(hormone_markers(df, col_map = cm, verbose = TRUE, na_action = "keep"))
  expect_equal(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

test_that("handles NA inputs gracefully", {
  skip_on_cran()
  df <- tibble(
    total_testosterone = NA, SHBG = 2,
    LH = 1, FSH = 1,
    estradiol = 1, progesterone = 1,
    TSH = 2, free_T3 = 1, free_T4 = 1,
    aldosterone = 1, renin = 1,
    insulin = 1, glucagon = 1,
    GH = 1, IGF1 = 1,
    prolactin = 1,
    cortisol_0 = 1, cortisol_30 = 1
  )
  cm <- setNames(as.list(names(df)), names(df))
  out <- hormone_markers(df, col_map = cm, na_action = "keep")
  expect_true(is.na(out$FAI))  # NA numerator propagates
  expect_equal(out$LH_FSH, 1 / 1)
})

test_that("inference: free_T3 derived from TSH + free_T4 enables T3_T4", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(
    total_testosterone = 10, SHBG = 2,
    estradiol = 100,
    TSH = 2, free_T4 = 10,       # free_T3 NOT mapped
    aldosterone = 20, renin = 4,
    IGF1 = 100,                   # GH NOT mapped
    cortisol_0 = 200, cortisol_30 = 260
  )
  cm <- setNames(as.list(names(df)), names(df))
  msgs <- testthat::capture_messages(
    out <- hormone_markers(df, col_map = cm, verbose = TRUE, na_action = "keep")
  )
  # T3_T4 should be present via inferred free_T3
  expect_true("T3_T4" %in% names(out))
  # GH_IGF1 should be present via inferred GH
  expect_true("GH_IGF1" %in% names(out))
  # E2_T uses estradiol + total_testosterone (both mapped)
  expect_true("E2_T" %in% names(out))
  # TSH_fT4 from TSH + free_T4 (both mapped)
  expect_true("TSH_fT4" %in% names(out))
  # Inference messages emitted
  expect_true(any(grepl("inferred", msgs)))
  # Attributes mark which keys were inferred
  expect_true(!is.null(attr(out, "inferred_inputs")))
})
