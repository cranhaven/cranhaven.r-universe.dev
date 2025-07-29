#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("makeRoundUp")
library(testthat)
test_that("makeRoundup correctly rounded values based on integer and modulus", {
  int1 <- makeRoundUp(int = 1L, modulus = 2L)
  expect_identical(int1, 2L)
  int1 <- makeRoundUp(int = 1L, modulus = 3L)
  expect_identical(int1, 3L)
  int1 <- makeRoundUp(int = 3L, modulus = 2L)
  expect_identical(int1, 4L)
  int1 <- makeRoundUp(int = 16L, modulus = 5L)
  expect_identical(int1, 20L)
})
