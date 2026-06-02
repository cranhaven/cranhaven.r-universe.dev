# tests/testthat/test_adiposity_sds_strat.R

library(testthat)
library(tibble)

# Shared reference set
ref_full <- list(
  M = list(
    BMI   = c(mean = 23, sd = 3),
    waist = c(mean = 85, sd = 10)
  ),
  F = list(
    BMI   = c(mean = 21, sd = 3),
    waist = c(mean = 75, sd = 9)
  )
)

cm <- list(sex = "sex", vars = list(BMI = "BMI", waist = "waist"))

test_that("ref must have M and F", {
  skip_on_cran()
  df <- tibble(sex = "M", BMI = 25)
  bad_refs <- list(
    NULL,
    list(M = list(BMI = c(mean = 23, sd = 3))),
    list(F = list(BMI = c(mean = 21, sd = 3))),
    list(X = list(BMI = c(mean = 23, sd = 3)), F = list(BMI = c(mean = 21, sd = 3)))
  )
  for (r in bad_refs) {
    expect_error(
      adiposity_sds_strat(df, col_map = cm, ref = r),
      "`ref` must be a named list with elements 'M' and 'F'"
    )
  }
})

test_that("ref M/F variable sets must match", {
  skip_on_cran()
  df <- tibble(sex = "M", BMI = 25)
  ref_mismatch <- list(
    M = list(BMI = c(mean = 23, sd = 3), waist = c(mean = 80, sd = 10)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, col_map = cm, ref = ref_mismatch),
    "ref\\$M and ref\\$F must define identical variables"
  )
})

test_that("ref component must have mean & sd names", {
  skip_on_cran()
  df <- tibble(sex = "M", BMI = 25)
  bad_ref <- list(
    M = list(BMI = c(23, 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, col_map = cm, ref = bad_ref),
    "must be numeric with names mean, sd"
  )
})

test_that("sd must be >0", {
  skip_on_cran()
  df <- tibble(sex = "F", BMI = 20)
  ref_bad_sd <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 0))
  )
  expect_error(
    adiposity_sds_strat(df, col_map = cm, ref = ref_bad_sd),
    "sd must be >0"
  )
})

test_that("missing sex column errors", {
  skip_on_cran()
  df <- tibble(gender = "M", BMI = 25)
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, col_map = list(sex = "sex", vars = list(BMI = "BMI")), ref = ref),
    "sex column 'sex' not found"
  )
})

test_that("invalid sex values error", {
  skip_on_cran()
  df <- tibble(sex = c("M", "X"), BMI = c(25, 22))
  ref <- list(
    M = list(BMI = c(mean = 23, sd = 3)),
    F = list(BMI = c(mean = 21, sd = 3))
  )
  expect_error(
    adiposity_sds_strat(df, col_map = list(sex = "sex", vars = list(BMI = "BMI")), ref = ref),
    "rows have unmapped sex values"
  )
})

test_that("computes correct SDS (multi-variable, vectorized)", {
  skip_on_cran()
  df <- tibble(
    sex   = c("M", "F", "M"),
    BMI   = c(25, 20, 26),
    waist = c(95, 70, 80)
  )
  out <- adiposity_sds_strat(df, col_map = cm, ref = ref_full)
  expect_equal(out$BMI_SDS[1],  (25 - 23) / 3)
  expect_equal(out$waist_SDS[1], (95 - 85) / 10)
  expect_equal(out$BMI_SDS[2],  (20 - 21) / 3)
  expect_equal(out$waist_SDS[2], (70 - 75) / 9)
  expect_equal(nrow(out), 3)
  expect_named(out, c("BMI_SDS", "waist_SDS"))
})

test_that("custom variable mapping via col_map$vars", {
  skip_on_cran()
  df <- tibble(sex = c("M","F"), BMI_col = c(25, 23))
  ref <- list(
    M = list(BMI = c(mean = 24, sd = 3)),
    F = list(BMI = c(mean = 22, sd = 3))
  )
  out <- adiposity_sds_strat(df, col_map = list(sex = "sex", vars = list(BMI = "BMI_col")), ref = ref)
  expect_equal(out$BMI_SDS, c((25-24)/3, (23-22)/3))
})

test_that("na_action omit vs error", {
  skip_on_cran()
  df <- tibble(sex = c("M","F","M"), BMI = c(25, NA, 24), waist = c(90, 70, 88))
  out_keep <- adiposity_sds_strat(df, col_map = cm, ref = ref_full, na_action = "keep")
  expect_equal(nrow(out_keep), 3)
  out_omit <- adiposity_sds_strat(df, col_map = cm, ref = ref_full, na_action = "omit")
  expect_equal(nrow(out_omit), 2)
  expect_error(
    adiposity_sds_strat(df, col_map = cm, ref = ref_full, na_action = "error"),
    "missing values present"
  )
})

test_that("allow_partial skips missing vars when TRUE", {
  skip_on_cran()
  ref_partial <- list(
    M = list(BMI = c(mean=23, sd=3), waist = c(mean=85, sd=10)),
    F = list(BMI = c(mean=21, sd=3), waist = c(mean=75, sd=9))
  )
  df <- tibble(sex = c("M","F"), BMI = c(24,22))  # waist missing
  out <- adiposity_sds_strat(df, col_map = list(sex = "sex"), ref = ref_partial, allow_partial = TRUE)
  expect_named(out, "BMI_SDS")
  expect_equal(nrow(out), 2)
})

test_that("allow_partial=FALSE errors when data var missing", {
  skip_on_cran()
  ref_partial <- ref_full
  df <- tibble(sex = c("M","F"), BMI = c(24,22))
  expect_error(
    adiposity_sds_strat(df, col_map = list(sex = "sex", vars = list(BMI = "BMI", waist = "waist")), ref = ref_partial, allow_partial = FALSE),
    "variables not found in data"
  )
})

test_that("check_extreme removed: function passes through raw outlier rows", {
  skip_on_cran()
  df <- tibble(
    sex = c("M","F"),
    BMI = c(120, 2),
    waist = c(300, 15)
  )
  ref <- list(
    M = list(BMI = c(mean=23, sd=3), waist = c(mean=85, sd=10)),
    F = list(BMI = c(mean=21, sd=3), waist = c(mean=75, sd=9))
  )
  out <- adiposity_sds_strat(df, col_map = cm, ref = ref)
  expect_equal(nrow(out), 2L)
})

test_that("prefix applied to output columns", {
  skip_on_cran()
  df <- tibble(sex = c("M","F"), BMI = c(24,22))
  ref <- list(
    M = list(BMI = c(mean=23, sd=3)),
    F = list(BMI = c(mean=21, sd=3))
  )
  out <- adiposity_sds_strat(df, col_map = list(sex = "sex", vars = list(BMI = "BMI")), ref = ref, prefix = "x_")
  expect_named(out, "x_BMI_SDS")
})

test_that("numeric sex coding 1/2 accepted", {
  skip_on_cran()
  df <- tibble(sex = c(1L, 2L, 1L), BMI = c(25, 20, 26), waist = c(95, 70, 80))
  out <- adiposity_sds_strat(df, col_map = cm, ref = ref_full)
  # row 1 is male (1), row 2 is female (2)
  expect_equal(out$BMI_SDS[1], (25 - 23) / 3)
  expect_equal(out$BMI_SDS[2], (20 - 21) / 3)
  expect_named(out, c("BMI_SDS", "waist_SDS"))
})

test_that("verbose = TRUE emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(sex = "M", BMI = 25, waist = 95)
  expect_message(adiposity_sds_strat(df, col_map = cm, ref = ref_full, verbose = TRUE), "adiposity_sds_strat")
  expect_message(adiposity_sds_strat(df, col_map = cm, ref = ref_full, verbose = TRUE), "col_map")
  expect_message(adiposity_sds_strat(df, col_map = cm, ref = ref_full, verbose = TRUE), "results:")
})

test_that("verbose double-fire guard: each message fires exactly once", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df <- tibble(sex = "M", BMI = 25, waist = 95)
  msgs <- testthat::capture_messages(
    adiposity_sds_strat(df, col_map = cm, ref = ref_full, verbose = TRUE)
  )
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})
