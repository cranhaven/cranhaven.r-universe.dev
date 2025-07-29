#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getPedMaxAge")
library(testthat)
library(lubridate)

pedOne <- nprcgenekeepr::pedOne
pedOne$age <- (mdy("10-05-2017", tz = "America/Chicago") -
  as.POSIXct(pedOne$birth)) / dyears(1L)

test_that("getPedMaxAge finds max age", {
  expect_equal(getPedMaxAge(pedOne), 17.227146, tolerance = 0.01)
  expect_equal(getPedMaxAge(pedOne[c(-1L, -2L), ]), 11.48742, tolerance = 0.01)
})
