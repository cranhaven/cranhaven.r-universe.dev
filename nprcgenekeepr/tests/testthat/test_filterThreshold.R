#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("filterThreshold")
library(testthat)
data("lacy1989Ped")
ped <- lacy1989Ped

ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
kin <- kinMatrix2LongForm(kmat, removeDups = FALSE)
threshold <- 0.1
kinFiltered <- filterThreshold(kin, threshold = threshold)

test_that("filterThreshold makes correct transformation", {
  expect_identical(nrow(kinFiltered), 39L)
  expect_identical(kinFiltered[1L, 3L], kin[1L, 3L])
  expect_identical(kinFiltered[2L, 3L], kin[3L, 3L])
  expect_identical(kinFiltered[4L, 3L], kin[6L, 3L])
  expect_identical(kinFiltered[11L, 3L], kin[15L, 3L])
  expect_identical(kinFiltered[24L, 3L], kin[34L, 3L])
  expect_identical(kinFiltered[1L, 2L], kin[1L, 2L])
  expect_identical(kinFiltered[2L, 2L], kin[3L, 2L])
  expect_identical(kinFiltered[4L, 2L], kin[6L, 2L])
  expect_identical(kinFiltered[11L, 2L], kin[15L, 2L])
  expect_identical(kinFiltered[24L, 2L], kin[34L, 2L])
})
