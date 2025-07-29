#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("meanKinship")
library(testthat)
set_seed(10L)
samp <- c(0.0, 0.5, 0.25, 0.125, 0.0625, 0.03125, 0.015625)
kmat <- sample(samp, 25L, replace = TRUE)
kmat <- matrix(kmat, nrow = 5L)

test_that("meanKinship averages column correctly", {
  expect_identical(kmat[[1L]], mean(kmat[[1L]]))
  expect_identical(kmat[[2L]], mean(kmat[[2L]]))
  expect_identical(kmat[[5L]], mean(kmat[[5L]]))
})
