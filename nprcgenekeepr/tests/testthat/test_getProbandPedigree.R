#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getProbandPedigree")
library(testthat)

data("lacy1989Ped")
ped <- lacy1989Ped
test_that("getProbandPedigree returns the correct pedigree", {
  expect_true(all(getProbandPedigree(probands = c("A", "B"), ped)$id %in%
    c("A", "B")))
  expect_true(all(getProbandPedigree(probands = c("A", "B", "E"), ped)$id %in%
    c("A", "B", "E")))
  expect_true(all(getProbandPedigree(probands = "F", ped)$id %in%
    c("A", "B", "D", "E", "F")))
  expect_true(all(c("A", "B", "D", "E", "F") %in%
    getProbandPedigree(probands = c("F"), ped)$id))
  expect_true(all(getProbandPedigree(probands = "D", ped)$id %in%
    c("A", "B", "D")))
  expect_true(all(c("A", "B", "D") %in%
    getProbandPedigree(probands = "D", ped)$id))
})
