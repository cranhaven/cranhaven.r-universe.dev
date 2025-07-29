#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getMaxAx")
test_that("getMaxAx correctly rounded values based on integer and modulus", {
  int1 <- nprcgenekeepr:::getMaxAx(
    bins = list(male = 11L, female = 5L),
    axModulus = 5L
  )
  expect_equal(int1, 15L)
  int1 <- nprcgenekeepr:::getMaxAx(
    bins = list(male = 21L, female = 5L),
    axModulus = 5L
  )
  expect_equal(int1, 25L)
  int1 <- nprcgenekeepr:::getMaxAx(
    bins = list(male = 11L, female = 15L),
    axModulus = 5L
  )
  expect_equal(int1, 20L)
  int1 <- nprcgenekeepr:::getMaxAx(
    bins = list(male = 11L, female = 203L),
    axModulus = 5L
  )
  expect_equal(int1, 205L)
  int1 <- nprcgenekeepr:::getMaxAx(
    bins = list(male = 101L, female = 5L),
    axModulus = 105L
  )
  expect_equal(int1, 105L)
})
