#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getGVPopulation")
library(testthat)
data("smallPed")
ped <- smallPed
ped$population <- getGVPopulation(ped, NULL)
test_that("getGVPopulation correctly sets the correct IDs in the population", {
  ped$population <- getGVPopulation(ped, NULL)
  expect_identical(ped$id[ped$population], ped$id)
  ped$population <- getGVPopulation(ped, c("A", "C"))
  expect_true(all(ped$id[ped$population] %in% c("A", "C")))
})
