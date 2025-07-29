#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getOffspring")
library(testthat)
data("smallPed")
test_that("getOffspring returns the correct IDs", {
  expect_identical(
    getOffspring(smallPed, ids = c("A", "B")),
    c("C", "D", "H", "I", "M")
  )
  expect_identical(
    getOffspring(smallPed, ids = c("M")),
    "P"
  )
  expect_identical(
    getOffspring(smallPed, ids = c("Q")),
    "A"
  )
})
