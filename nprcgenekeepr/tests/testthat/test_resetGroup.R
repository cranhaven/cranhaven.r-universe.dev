#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("resetGroup")
library(testthat)
data("smallPed")
ped <- smallPed
test_that("resetGroup correctly returns correct IDs", {
  ped1 <- resetGroup(ped = ped, ids = NULL)
  expect_true(!any(ped1$group))
  ped1 <- resetGroup(ped = ped, ids = c("A", "B", "I"))
  expect_true(all(ped1$group[ped1$id %in%
    c("A", "B", "I")]))
  expect_length(setdiff(ped1$id[ped1$group], c("A", "B", "I")), 0L)
})
