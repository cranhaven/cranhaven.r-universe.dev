#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("findPedigreeNumber")
library(stringi)
data("lacy1989Ped")
ped <- lacy1989Ped
ped$gen <- NULL
ped$population <- NULL
ped2 <- ped
ped2$id <- stri_c(ped$id, "2")
ped2$sire <- stri_c(ped$sire, "2")
ped2$dam <- stri_c(ped$dam, "2")
ped3 <- ped
ped3$id <- stri_c(ped$id, "3")
ped3$sire <- stri_c(ped$sire, "3")
ped3$dam <- stri_c(ped$dam, "3")
ped <- rbind(ped, ped2)
ped <- rbind(ped, ped3)
test_that("findPedigreeNumber identifies separate pedigrees correctly", {
  ped$pedigree <- findPedigreeNumber(ped$id, ped$sire, ped$dam)
  expect_true(all(ped$pedigree[stri_detect_fixed(ped$id, "1")] == 1L))
  expect_true(all(ped$pedigree[stri_detect_fixed(ped$id, "2")] == 2L))
  expect_true(all(ped$pedigree[stri_detect_fixed(ped$id, "3")] == 3L))
})
