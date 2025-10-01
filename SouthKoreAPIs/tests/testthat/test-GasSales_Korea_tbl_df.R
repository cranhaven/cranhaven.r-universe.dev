# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# GasSales_Korea_tbl_df

library(testthat)

test_that("GasSales_Korea_tbl_df is a spec_tbl_df tibble", {
  expect_s3_class(GasSales_Korea_tbl_df, "spec_tbl_df")
  expect_s3_class(GasSales_Korea_tbl_df, "tbl_df")
  expect_s3_class(GasSales_Korea_tbl_df, "tbl")
  expect_s3_class(GasSales_Korea_tbl_df, "data.frame")
})

test_that("GasSales_Korea_tbl_df has 21 columns", {
  expect_equal(length(GasSales_Korea_tbl_df), 21)
})

test_that("GasSales_Korea_tbl_df has 252 rows", {
  expect_equal(nrow(GasSales_Korea_tbl_df), 252)
})

test_that("GasSales_Korea_tbl_df has correct column names", {
  expect_named(GasSales_Korea_tbl_df, c(
    "Year", "Month", "Temperature", "Gangwondo", "Seoul", "Gyeonggido", "Incheon",
    "Gyeongsangnamdo", "Gyeongsangbukdo", "Gwangju", "Daegu", "Daejeon", "Busan",
    "Sejong", "Ulsan", "Jeollanamdo", "Jeollabukdo", "Jeju",
    "Chungcheongnamdo", "Chungcheongbukdo", "Sum"
  ))
})

test_that("GasSales_Korea_tbl_df columns have correct types", {
  expect_type(GasSales_Korea_tbl_df$Year, "double")
  expect_type(GasSales_Korea_tbl_df$Month, "double")
  expect_type(GasSales_Korea_tbl_df$Temperature, "double")
  expect_type(GasSales_Korea_tbl_df$Gangwondo, "double")
  expect_type(GasSales_Korea_tbl_df$Seoul, "double")
  expect_type(GasSales_Korea_tbl_df$Gyeonggido, "double")
  expect_type(GasSales_Korea_tbl_df$Incheon, "double")
  expect_type(GasSales_Korea_tbl_df$Gyeongsangnamdo, "double")
  expect_type(GasSales_Korea_tbl_df$Gyeongsangbukdo, "double")
  expect_type(GasSales_Korea_tbl_df$Gwangju, "double")
  expect_type(GasSales_Korea_tbl_df$Daegu, "double")
  expect_type(GasSales_Korea_tbl_df$Daejeon, "double")
  expect_type(GasSales_Korea_tbl_df$Busan, "double")
  expect_type(GasSales_Korea_tbl_df$Sejong, "double")
  expect_type(GasSales_Korea_tbl_df$Ulsan, "double")
  expect_type(GasSales_Korea_tbl_df$Jeollanamdo, "double")
  expect_type(GasSales_Korea_tbl_df$Jeollabukdo, "double")
  expect_type(GasSales_Korea_tbl_df$Jeju, "double")
  expect_type(GasSales_Korea_tbl_df$Chungcheongnamdo, "double")
  expect_type(GasSales_Korea_tbl_df$Chungcheongbukdo, "double")
  expect_type(GasSales_Korea_tbl_df$Sum, "double")
})
