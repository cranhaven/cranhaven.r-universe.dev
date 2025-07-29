#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("setPopulation")
library(testthat)
data("smallPed")
ped <- smallPed
test_that("setPopulation sets correct records to TRUE and FALSE", {
  ped1 <- setPopulation(ped = ped, ids = NULL)
  expect_true(all(ped1$population))
  ped1 <- setPopulation(ped = ped, ids = c("A", "B", "I"))
  expect_true(all(ped1$population[ped1$id %in%
    c("A", "B", "I")]))
  expect_length(setdiff(ped1$id[ped1$population], c("A", "B", "I")),
                   0L)
})
