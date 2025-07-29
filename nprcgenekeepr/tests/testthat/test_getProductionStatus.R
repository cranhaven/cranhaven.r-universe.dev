#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getProductionStatus")
library(testthat)
library(nprcgenekeepr)
data("examplePedigree")
ped <- examplePedigree
minParentAge <- 3.0

test_that("getProductionStatus calculates correctly", {
  status <- getProductionStatus(
    ped,
    minParentAge = minParentAge, maxOffspringAge = NULL, housing = "shelter_pens",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_equal(status$production, 0.112877583465819)
  expect_equal(status$color, "red")
})
test_that("getProductionStatus calculates correctly", {
  status <- getProductionStatus(
    ped,
    minParentAge = minParentAge, maxOffspringAge = NULL, housing = "corral",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_equal(status$production, 0.112877583465819)
  expect_equal(status$color, "red")
})
## These are the IDs from the test above
ids <- c(
  "Q3J24E", "IC716S", "SJD499", "YJBY17", "5C63F4", "R3R07C",
  "MPAL6H", "X0EFTS", "LRD33X", "45V0XG", "HUJG4E", "W40T5A", "2HDW5H",
  "W17RHK", "1X5A92", "9AC7VI", "KR9CKT", "3WABPU", "8LK8R4", "FGPCWH",
  "JQJ227", "BK22E3", "CTVG9A", "QVJU41", "4G3GET", "LNVD11", "IJFU2U",
  "DLKEI5", "MGA3UT", "YVCTVD", "MDRA94", "RJ7H7T", "3921MR", "4Z5SNV",
  "ZYTRMM", "HYSW4M", "KTKPNB", "MDBYYE", "0U7WIE", "W07ANU", "9Y64R9",
  "BSDDEP", "7MEQMP", "LXJFKL", "KR0VFP", "KYFID0", "TEN5YM", "MNV5GE",
  "SPIZM7", "MM8MYL", "SSJDJE", "CS6U5F", "C6URM1", "N322YA", "6XV95Z",
  "NQSMBG", "2X4K5B", "E0ACPZ", "FCLNFN", "LVLLL5", "ZLQYFT", "NP4Q3H",
  "HWNWV4", "FCYQGS", "XWR0YW", "XIKCDK", "BWE7N8", "H6T2FF", "X994RC",
  "I04JZV", "ZKKR4C"
)
pedWith71 <- ped[ped$id %in% ids, ]
removeIDs <- c("Q3J24E", "IC716S", "SJD499", "YJBY17", "5C63F4", "R3R07C")
test_that("getProductionStatus calculates correctly", {
  status <- getProductionStatus(
    pedWith71,
    minParentAge = minParentAge, maxOffspringAge = NULL,
    housing = "shelter_pens",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_equal(status$production, 23.6666666666667)
  expect_equal(status$color, "green")
})
noDamsPed <- pedWith71[!pedWith71$id %in% ped$id[ped$sex == "F" &
  ped$age >= minParentAge], ]
test_that("getProductionStatus detects no dams", {
  status <- getProductionStatus(
    noDamsPed,
    minParentAge = minParentAge, maxOffspringAge = NULL,
    housing = "shelter_pens",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_true(is.na(status$production))
  expect_equal(status$color, "green")
})
test_that("getProductionStatus detects no dams", {
  status <- getProductionStatus(
    noDamsPed,
    minParentAge = minParentAge, maxOffspringAge = NULL,
    housing = "corral",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_true(is.na(status$production))
  expect_equal(status$color, "green")
})
test_that("getProductionStatus calculates correctly", {
  status <- getProductionStatus(
    pedWith71,
    minParentAge = minParentAge, maxOffspringAge = NULL,
    housing = "corral",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_equal(status$production, 23.6666666666667)
  expect_equal(status$color, "green")
})
dams <- ped$dam[ped$id %in% ids]
pedWith71OffspringWithDams <- ped[ped$id %in% c(ids, dams), ]
test_that("getProductionStatus calculates correctly", {
  status <- getProductionStatus(
    pedWith71OffspringWithDams,
    minParentAge = minParentAge, maxOffspringAge = NULL,
    housing = "shelter_pens",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_equal(status$production, 1.12698412698413)
  expect_equal(status$color, "green")
})
badPed <- pedWith71[, -1L]
test_that("getProductionStatus detects missing column", {
  expect_error(
    getProductionStatus(
      badPed,
      minParentAge = minParentAge, maxOffspringAge = NULL,
      housing = "shelter_pens",
      currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
    ),
    "ped is missing: id"
  )
})
test_that("getProductionStatus detects wrong housing type", {
  expect_error(
    getProductionStatus(
      pedWith71OffspringWithDams,
      minParentAge = minParentAge, maxOffspringAge = NULL,
      housing = "bad housing",
      currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
    ),
    "Undefined housing type in getProduction status is: bad housing"
  )
})
yellowCorralPed <- pedWith71OffspringWithDams[!
pedWith71OffspringWithDams$id %in% ids[1:39], ]
test_that("getProductionStatus calculates correctly", {
  status <- getProductionStatus(
    yellowCorralPed,
    minParentAge = minParentAge, maxOffspringAge = NULL,
    housing = "corral",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_equal(status$production, 0.516, tolerance = 0.001)
  expect_equal(status$color, "yellow")
})
yellowShelterPedPed <- pedWith71OffspringWithDams[
  !pedWith71OffspringWithDams$id %in% ids[1L:33L],
]
test_that("getProductionStatus calculates correctly", {
  status <- getProductionStatus(
    yellowShelterPedPed,
    minParentAge = minParentAge, maxOffspringAge = NULL,
    housing = "shelter_pens",
    currentDate = as.Date("2010-10-10", format = "%Y-%m-%d")
  )
  expect_equal(status$production, 0.61290322580652)
  expect_identical(status$color, "yellow")
})
