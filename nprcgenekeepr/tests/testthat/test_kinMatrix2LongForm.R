#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("kinMatrix2LongForm")
library(testthat)

ped <- nprcgenekeepr::lacy1989Ped
ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
reformattedKmat <- kinMatrix2LongForm(kmat, removeDups = FALSE)
reformattedNoDupsKmat <- kinMatrix2LongForm(kmat, removeDups = TRUE)

test_that("kinMatrix2LongForm makes correct transformation", {
  expect_identical(reformattedKmat[1L, 3L], as.numeric(kmat[1L, 1L]))
  expect_identical(reformattedKmat[3L, 3L], as.numeric(kmat[1L, 3L]))
  expect_identical(reformattedKmat[5L, 3L], as.numeric(kmat[1L, 5L]))
  expect_identical(reformattedKmat[10L, 3L], as.numeric(kmat[2L, 3L]))
  expect_identical(reformattedNoDupsKmat[9L, 3L], as.numeric(kmat[2L, 3L]))
})
