#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("filterPairs")
library(testthat)
data("lacy1989Ped")
ped <- lacy1989Ped

ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
kin <- kinMatrix2LongForm(kmat, removeDups = FALSE)
threshold <- 0.1
kin <- filterThreshold(kin, threshold = threshold)
ped$sex <- c("M", "F", "M", "M", "F", "F", "M")
kinNull <- filterPairs(kin, ped, ignore = NULL)
kinFF <- filterPairs(kin, ped, ignore = list(c("F", "F")))
kinMM <- filterPairs(kin, ped, ignore = list(c("M", "M")))

test_that("filterPairs removes the correct pairs", {
  expect_identical(nrow(kinNull), 39L)
  expect_identical(nrow(kinFF), 32L)
  expect_identical(nrow(kinMM), 23L)
  expect_identical(nrow(kinFF[kinFF$id1 == "B" & kinFF$id2 == "E", ]), 0L)
  expect_identical(nrow(kinFF[kinFF$id1 == "B" & kinFF$id2 == "F", ]), 0L)
  expect_identical(nrow(kinMM[kinMM$id1 == "A" & kinMM$id2 == "D", ]), 0L)
})
