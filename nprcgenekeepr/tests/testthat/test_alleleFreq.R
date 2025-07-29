#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("alleleFreq")
library(testthat)
data("ped1Alleles")
ids <- ped1Alleles$id
alleles <- ped1Alleles[, !(names(ped1Alleles) %in% c("id", "parent"))]
test_that("alleleFreq forms dataframe with correct calculations", {
  aF <- alleleFreq(alleles[[1L]], ids = NULL)
  expect_identical(aF$freq[aF$allele == 20004L], 10L)
  expect_identical(aF$freq[aF$allele == 20004L], 10L)
  expect_identical(aF$freq[aF$allele == 20012L], 11L)
  aF <- alleleFreq(alleles[[4L]], ids = NULL)
  expect_identical(aF$freq[aF$allele == 20004L], 14L)
  expect_identical(aF$freq[aF$allele == 20012L], 9L)
  aF <- alleleFreq(ped1Alleles[[1L]], ids = ids)
  expect_identical(aF$freq[aF$allele == 20004L], 10L)
  expect_identical(aF$freq[aF$allele == 20012L], 10L)
  aF <- alleleFreq(ped1Alleles[[4L]], ids = ids)
  expect_identical(aF$freq[aF$allele == 20004L], 13L)
  expect_identical(aF$freq[aF$allele == 20012L], 9L)
})
