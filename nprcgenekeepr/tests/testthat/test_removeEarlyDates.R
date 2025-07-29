#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("removeEarlyDates")
library(testthat)

dates <- structure(c(
  12361L, 14400L, 15413L, NA, 11189L, NA, 13224L, 10971L, -432000L,
  +13262L
), class = "Date")
result <- structure(c(
  12361L, 14400L, 15413L, NA, 11189L, NA, 13224L, 10971L, NA,
  +13262L
), class = "Date")
test_that("removeEarlyDates changes early dates to NA", {
  expect_equal(removeEarlyDates(dates, firstYear = 1000L), result)
})
