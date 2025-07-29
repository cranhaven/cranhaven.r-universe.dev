#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("kinship")
library(testthat)
data("lacy1989Ped")
ped <- lacy1989Ped

test_that("kinship makes correct calculations", {
  kmatSparse <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = TRUE)
  kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
  expect_identical(as.numeric(kmatSparse), as.numeric(kmat))
  expect_equal(kmat[1L, 1L], 0.5)
  expect_equal(kmat[1L, 3L], 0.25)
  expect_equal(kmat[1L, 5L], 0.0)
  expect_equal(kmat[1L, 6L], 0.125)
  expect_equal(kmat[1L, 2L], 0.0)
  expect_equal(kmat[6L, 2L], 0.125)
})
ped <- rbind(ped, ped[1L, ])
test_that("kinship detects duplicate record", {
  expect_error(kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = TRUE),
    "All id values must be unique",
    fixed = TRUE
  )
})
