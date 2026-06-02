library(testthat)
library(HealthMarkers)

# ---- marker_summary ---------------------------------------------------------

test_that("marker_summary returns tibble with variable/mean/sd/iqr", {
  df <- data.frame(glucose = c(5.5, 6.1, 4.9), insulin = c(60, 88, 55),
                   bmi = c(24, 27, 22))
  out <- marker_summary(df)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("variable", "mean", "sd", "iqr"))
  expect_equal(nrow(out), 3L)
  expect_setequal(out$variable, c("glucose", "insulin", "bmi"))
})

test_that("marker_summary returns correct values", {
  skip_on_cran()
  df <- data.frame(x = c(1, 2, 3))
  out <- marker_summary(df)
  expect_equal(out$mean, 2)
  expect_equal(out$sd, 1, tolerance = 1e-10)
  expect_equal(out$iqr, 1)
})

test_that("marker_summary ignores non-numeric columns", {
  skip_on_cran()
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  out <- marker_summary(df)
  expect_equal(nrow(out), 1L)
  expect_equal(out$variable, "a")
})

test_that("marker_summary returns empty tibble when no numeric columns", {
  skip_on_cran()
  df <- data.frame(a = c("x", "y"), stringsAsFactors = FALSE)
  out <- marker_summary(df)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
})

test_that("marker_summary handles NA values (mean/sd/iqr computed na.rm=TRUE)", {
  skip_on_cran()
  df <- data.frame(x = c(1, NA, 3))
  out <- marker_summary(df)
  expect_equal(out$mean, 2)
  expect_false(is.na(out$mean))
})

test_that("marker_summary errors on non-data.frame input", {
  skip_on_cran()
  expect_error(marker_summary(1:5), "must be a data.frame or tibble")
})

test_that("marker_summary verbose=TRUE runs without error", {
  skip_on_cran()
  df <- data.frame(a = 1:3)
  expect_no_error(marker_summary(df, verbose = TRUE))
})
