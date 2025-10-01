# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# get_southkorea_gdp

library(testthat)

test_that("get_southkorea_gdp() returns a tibble with the correct structure and content", {
  result <- get_southkorea_gdp()

  # Check that the result is not NULL
  expect_false(is.null(result))

  # Check that the result is a data.frame/tibble
  expect_s3_class(result, "data.frame")

  # Check that the column names are exactly as expected
  expect_named(result, c("indicator", "country", "year", "value", "value_label"))

  # Check data types of each column
  expect_type(result$indicator, "character")
  expect_type(result$country, "character")
  expect_type(result$year, "integer")
  expect_type(result$value, "double")
  expect_type(result$value_label, "character")

  # Check that the indicator column contains only the expected value
  expect_true(all(result$indicator == "GDP (current US$)"))

  # Check that the country column contains only "Korea, Rep."
  expect_true(all(result$country == "Korea, Rep."))

  # Check that the year range is correct
  expect_true(all(result$year >= 2010 & result$year <= 2022))

  # Check that the number of rows is 13 (2010–2022)
  expect_equal(nrow(result), 13)

  # Check that there are no NA values in the value column
  expect_false(any(is.na(result$value)))

  # Check that value_label is formatted with commas
  expect_true(all(grepl(",", result$value_label)))
})
