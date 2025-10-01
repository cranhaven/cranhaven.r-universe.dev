# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_country_info_kr

library(testthat)

test_that("get_country_info_kr() returns a tibble with correct structure", {
  skip_on_cran()  # Skip test on CRAN if the API is unavailable

  result <- get_country_info_kr()

  # Skip if no data is returned
  skip_if(is.null(result), "API unavailable, test skipped")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("name_common", "name_official", "region", "subregion",
                         "capital", "area", "population", "languages"))
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 8)
})

test_that("get_country_info_kr() returns correct data types", {
  skip_on_cran()

  result <- get_country_info_kr()
  skip_if(is.null(result), "API unavailable, test skipped")

  expect_type(result$name_common, "character")
  expect_type(result$name_official, "character")
  expect_type(result$region, "character")
  expect_type(result$subregion, "character")
  expect_type(result$capital, "character")
  expect_type(result$area, "double")
  expect_type(result$population, "integer")
  expect_type(result$languages, "character")
})

test_that("get_country_info_kr() returns expected values", {
  skip_on_cran()

  result <- get_country_info_kr()
  skip_if(is.null(result), "API unavailable, test skipped")

  expect_equal(result$name_common, "South Korea")
  expect_equal(result$name_official, "Republic of Korea")
  expect_equal(result$region, "Asia")
  expect_equal(result$subregion, "Eastern Asia")
  expect_equal(result$capital, "Seoul")
  expect_true(result$area > 90000)
  expect_true(result$population > 50000000)
  expect_true(grepl("Korean", result$languages))
})
