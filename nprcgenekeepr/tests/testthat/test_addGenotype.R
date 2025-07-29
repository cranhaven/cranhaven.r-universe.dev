#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("addGenotype")
library(testthat)
library(stringi)
qcPed <- nprcgenekeepr::qcPed

qcPed <- qcPed[order(qcPed$id), ]
ped <- qcPed
genotype <- data.frame(
  id = ped$id[50L + 1L:20L],
  first_name = stri_c("first", 1L:20L),
  second_name = stri_c("second", 1L:20L),
  stringsAsFactors = FALSE
)

test_that("addGenotype forms correct dataframe", {
  newPed <- addGenotype(ped, genotype)
  newPed <- newPed[order(newPed$id), ]
  expect_identical(as.character(newPed$first[newPed$id == ped$id[50L + 1L]]),
                   "10001")
  expect_identical(
    as.character(newPed$second[newPed$id == ped$id[50L + 1L]]),
    "10021"
  )
  expect_identical(
    as.character(newPed$first[newPed$id == ped$id[50L + 2L]]),
    "10012"
  )
  expect_identical(
    as.character(newPed$second[newPed$id == ped$id[50L + 2L]]),
    "10032"
  )
})
