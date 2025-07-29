#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
set_seed(1)
vec <- abs(rnorm(10L))

test_that("getProportionLow returns the correct values", {
  lowVec <- ifelse(vec > 0.3, "High", "Low")
  expect_identical(
    getProportionLow(lowVec),
    list(proportion = 0.1, color = "green", colorIndex = 3L)
  )
  lowVec <- ifelse(vec > 0.4, "High", "Low")
  expect_identical(
    getProportionLow(lowVec),
    list(proportion = 0.3, color = "yellow", colorIndex = 2L)
  )
  lowVec <- ifelse(vec > 0.7, "High", "Low")
  expect_identical(
    getProportionLow(lowVec),
    list(proportion = 0.6, color = "red", colorIndex = 1L)
  )
})
