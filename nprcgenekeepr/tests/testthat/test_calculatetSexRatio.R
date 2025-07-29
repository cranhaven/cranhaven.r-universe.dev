#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("calculateSexRatio")

data("qcBreeders")
data("pedWithGenotype")
skip_if_not(exists("qcBreeders"))
skip_if_not(exists("pedWithGenotype"))
available <- c(
  "JGPN6K", "8KM1MP", "I9TQ0T", "Q0RGP7", "VFS0XB", "CQC133",
  "2KULR3", "HOYW0S", "FHV13N", "OUM6QF", "6Z7MD9", "CFPEEU",
  "HLI95R", "RI0O7F", "7M51X5", "DR5GXB", "170ZTZ", "C1ICXL"
)
nonMales <- c(
  "JGPN6K", "8KM1MP", "I9TQ0T", "Q0RGP7", "CQC133",
  "2KULR3", "HOYW0S", "FHV13N", "OUM6QF", "6Z7MD9", "CFPEEU",
  "HLI95R", "RI0O7F", "7M51X5", "DR5GXB", "170ZTZ", "C1ICXL"
)
male <- "VFS0XB"
test_that("calculateSexRatio calculates correctly", {
  expect_equal(calculateSexRatio(ids = male, ped = pedWithGenotype), 0.0)
  expect_equal(calculateSexRatio(ids = nonMales, ped = pedWithGenotype), Inf)
  expect_equal(calculateSexRatio(ids = available, ped = pedWithGenotype), 17.0)
  expect_equal(calculateSexRatio(
    ids = available, ped = pedWithGenotype,
    additionalMales = 1L
  ), 8.5)
  expect_equal(calculateSexRatio(
    ids = available, ped = pedWithGenotype,
    additionalFemales = 1L
  ), 18.0)
  expect_equal(calculateSexRatio(
    ids = available, ped = pedWithGenotype,
    additionalMales = 1L, additionalFemales = 1L
  ), 9.0)
  expect_equal(
    calculateSexRatio(
      ids = nonMales, ped = pedWithGenotype,
      additionalMales = 1L, additionalFemales = 0L
    ),
    17.0
  )
  expect_equal(
    calculateSexRatio(
      ids = character(0L), ped = pedWithGenotype,
      additionalMales = 1L, additionalFemales = 0L
    ),
    0.0
  )
  expect_true(is.na(calculateSexRatio(
    ids = character(0L), ped = pedWithGenotype,
    additionalMales = 0L,
    additionalFemales = 0L
  )))
  expect_equal(calculateSexRatio(
    ids = character(0L), ped = pedWithGenotype,
    additionalMales = 0L,
    additionalFemales = 1L
  ), Inf)
  expect_equal(calculateSexRatio(
    ids = character(0L), ped = pedWithGenotype,
    additionalMales = 2L,
    additionalFemales = 1L
  ), 0.5)
})
