#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("obfuscatePed")
library(testthat)

test_that("obfuscatePed creates correctly obfuscated pedigree", {
  pedSix <- qcStudbook(nprcgenekeepr::pedSix)
  ped <- obfuscatePed(pedSix, size = 3L, maxDelta = 20L)
  expect_identical(nrow(ped), nrow(pedSix))
  expect_identical(ncol(ped), ncol(pedSix))
  expect_identical(ped$id[1L], ped$dam[7L])
  expect_identical(ped$id[1L], ped$dam[7L])
  expect_true(all(ped$id[2L] == ped$dam[c(8L, 11L, 12L)]))
  expect_true(max(abs(pedSix$birth[!is.na(pedSix$birth)] -
    ped$birth[!is.na(ped$birth)])) <= 20L)
})
test_that("obfuscatePed creates ID map on request", {
  pedSix <- qcStudbook(nprcgenekeepr::pedSix)
  ped <- obfuscatePed(pedSix, size = 3L, maxDelta = 20L, map = TRUE)
  expect_true(class(ped) == "list")
  expect_named(ped, c("ped", "map"))
  expect_s3_class(ped$ped, "data.frame")
  expect_identical(class(ped$map), "character")
  expect_named(ped$map, pedSix$id)
  expect_identical(as.character(ped$map), ped$ped$id)
})
