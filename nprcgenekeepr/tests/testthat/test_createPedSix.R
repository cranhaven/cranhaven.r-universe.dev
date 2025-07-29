#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("createPedSix")
library(testthat)
pedSix <- createPedSix(savePed = FALSE)
test_that("createPedSix makes the right pedigree", {
  expect_identical(nrow(pedSix), 8L)
  expect_identical(ncol(pedSix), 7L)
  expect_identical(names(pedSix)[1L], "Ego Id")
})
pedSix <- createPedSix(savePed = TRUE)
test_that("createPedSix makes the right pedigree and saves it", {
  expect_identical(names(pedSix)[1L], "Ego Id")
})
