# tests/testthat/test_inflammatory_age.R

library(testthat)
library(tibble)

# Fixture data
df <- tibble(
  CRP  = c(1.0, 2.0, NA_real_),
  IL6  = c(5.0, NA_real_, 3.0),
  TNFa = c(1.5, 2.5, 3.5)
)
col_map <- list(CRP = "CRP", IL6 = "IL6", TNFa = "TNFa")

# 1) Default na_action='keep': NAs propagate to output
test_that("iAge default na_action='keep' propagates NA", {
  skip_on_cran()
  out <- iAge(df, col_map = col_map)
  # Row 2: IL6 is NA -> iAge should be NA
  expect_true(is.na(out$iAge[2]))
  # Row 3: CRP is NA -> iAge should be NA
  expect_true(is.na(out$iAge[3]))
  # Row 1: all present -> numeric
  expect_equal(out$iAge[1], 0.33 * 1.0 + 0.33 * 5.0 + 0.34 * 1.5)
})

# 1b) With na_action='omit': NAs treated as 0
test_that("iAge computes correct weighted sum with na_action='omit'", {
  skip_on_cran()
  out <- suppressWarnings(iAge(df, col_map = col_map, na_action = "omit"))
  expected <- rowSums(cbind(
    0.33 * df$CRP,
    0.33 * df$IL6,
    0.34 * df$TNFa
  ), na.rm = TRUE)
  expect_s3_class(out, "tbl_df")
  expect_named(out, "iAge")
  expect_equal(out$iAge, expected)
})

# 2) Custom weights
test_that("iAge respects custom weights and sums to correct values", {
  skip_on_cran()
  w <- c(CRP = 0.5, IL6 = 0.3, TNFa = 0.2)
  out2 <- suppressWarnings(iAge(df, col_map = col_map, weights = w, na_action = "omit"))
  expected2 <- rowSums(cbind(
    0.5 * df$CRP,
    0.3 * df$IL6,
    0.2 * df$TNFa
  ), na.rm = TRUE)
  expect_equal(out2$iAge, expected2)
})

# 3) NA handling: if all markers NA for a row, sum gives 0 (omit)
test_that("iAge NA handling gives zero for all-NA row with na_action='omit'", {
  skip_on_cran()
  df_na <- tibble(CRP = NA_real_, IL6 = NA_real_, TNFa = NA_real_)
  out3 <- suppressWarnings(iAge(df_na, col_map = col_map, na_action = "omit"))
  expect_equal(out3$iAge, 0)
})

# 4) Verbose message
test_that("iAge verbose emits col_map and results messages", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df_ok <- tibble(CRP = 1, IL6 = 2, TNFa = 3)
  expect_message(iAge(df_ok, col_map = col_map, verbose = TRUE), "iAge")
  expect_message(iAge(df_ok, col_map = col_map, verbose = TRUE), "col_map")
  expect_message(iAge(df_ok, col_map = col_map, verbose = TRUE), "results:")
})

test_that("iAge verbose double-fire guard", {
  skip_on_cran()
  withr::local_options(healthmarkers.verbose = "inform")
  df_ok <- tibble(CRP = 1, IL6 = 2, TNFa = 3)
  msgs <- testthat::capture_messages(iAge(df_ok, col_map = col_map, verbose = TRUE))
  expect_gte(sum(grepl("col_map", msgs)), 1L)
  expect_equal(sum(grepl("results:",   msgs)), 1L)
})

# 5) Errors on invalid inputs
test_that("iAge errors on non-data.frame input", {
  skip_on_cran()
  expect_error(iAge("not_df", col_map), "data.frame or tibble")
})

test_that("partial col_map is supplemented by dictionary inference", {
  skip_on_cran()
  bad_map <- list(CRP = "CRP", IL6 = "IL6")
  expect_no_error(iAge(df, col_map = bad_map))
})

test_that("iAge errors on non-numeric weights", {
  skip_on_cran()
  expect_error(
    iAge(df, col_map = col_map, weights = c(CRP = "a", IL6 = 0.5, TNFa = 0.5)),
    "weights.*numeric"
  )
})

test_that("iAge errors on weights not summing to 1", {
  skip_on_cran()
  w2 <- c(CRP = 0.2, IL6 = 0.2, TNFa = 0.2)
  expect_error(
    iAge(df, col_map = col_map, weights = w2),
    "weights.*sum to 1"
  )
})

# 6) Errors on missing columns in data
test_that("iAge errors if data is missing required columns", {
  skip_on_cran()
  df_bad <- tibble(CRP = 1, IL6 = 2)
  expect_error(iAge(df_bad, col_map = col_map), "column 'TNFa'.*not found in data")
})

# 7) Errors if mapped column is non-numeric
test_that("iAge errors if mapped column is not numeric", {
  skip_on_cran()
  df_char <- tibble(CRP = c("x"), IL6 = 2, TNFa = 3)
  expect_error(iAge(df_char, col_map = col_map), "must be numeric")
})
