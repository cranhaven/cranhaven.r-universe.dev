#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("set_seed")

test_that("set_seed handle R versions < 3.6", {
  local_mocked_bindings(
    R_version = function() {
      list(major = "4", minor = "4.2")
    }
  )
  newSeed <- set_seed(1L)
  local_mocked_bindings(
    R_version = function() {
      list(major = "3", minor = "5.3")
    }
  )
  oldSeed <- set_seed(1L)

  expect_identical(newSeed, oldSeed)
})
