#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("filterAge")
library(testthat)

ped <- nprcgenekeepr::qcPed
ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen,
  sparse = FALSE
)
kin <- kinMatrix2LongForm(kmat)
filteredKin1 <- filterAge(kin, ped, minAge = 1L)
filteredKin5 <- filterAge(kin, ped, minAge = 5L)
lessThan5 <- c(
  "C1ICXL", "2KULR3", "RI0O7F", "7M51X5", "170ZTZ", "Y7PPEZ",
  "CFPEEU", "ZC5SCR", "218FOV", "2IXJ2N", "CAST4W", "JGPN6K",
  "HOYW0S", "DD1U77", "0DAV0I", "HLI95R", "TZ5NUB", "DR5GXB",
  "EUG3WE", "FHV13N", "OUM6QF", "6Z7MD9", "309VM2", "8KM1MP",
  "I9TQ0T", "INGWI7"
)
test_that("filterAge removes the correct rows", {
  expect_false(any("Y7PPEZ" %in% filteredKin1$id1))
  expect_true(any("C1ICXL" %in% filteredKin1$id1))
  expect_false(any("C1ICXL" %in% filteredKin5$id1))
})
