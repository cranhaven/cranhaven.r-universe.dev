#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("saveDataframesAsFiles")
library(testthat)
dfList <- list(
  lacy1989Ped = nprcgenekeepr::lacy1989Ped,
  pedGood = nprcgenekeepr::pedGood
)

test_that("makeExamplePedigreeFile creates CSV files", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  files_csv <- saveDataframesAsFiles(dfList,
    baseDir = tempdir(),
    fileType = "csv"
  )
  # nolint start: object_name_linter.
  pedCsv_1 <- read.table(files_csv[1L],
    sep = ",", header = TRUE,
    stringsAsFactors = FALSE
  )
  expect_named(pedCsv_1, names(nprcgenekeepr::lacy1989Ped))
  expect_identical(
    row.names.data.frame(pedCsv_1),
    row.names.data.frame(nprcgenekeepr::lacy1989Ped)
  )
  pedCsv_2 <- read.table(files_csv[2L],
    sep = ",", header = TRUE,
    stringsAsFactors = FALSE
  )
  expect_named(pedCsv_2, names(nprcgenekeepr::pedGood))
  expect_identical(
    row.names.data.frame(pedCsv_2),
    row.names.data.frame(nprcgenekeepr::pedGood)
  )
})
test_that("makeExamplePedigreeFile creates TXT files", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  files_csv <- saveDataframesAsFiles(dfList,
    baseDir = tempdir(),
    fileType = "txt"
  )
  pedCsv_1 <- read.table(files_csv[1L],
    sep = "\t", header = TRUE,
    stringsAsFactors = FALSE
  )
  expect_named(pedCsv_1, names(nprcgenekeepr::lacy1989Ped))
  expect_identical(
    row.names.data.frame(pedCsv_1),
    row.names.data.frame(nprcgenekeepr::lacy1989Ped)
  )
  pedCsv_2 <- read.table(files_csv[2L],
    sep = "\t", header = TRUE,
    stringsAsFactors = FALSE
  )
  expect_named(pedCsv_2, names(nprcgenekeepr::pedGood))
  expect_identical(
    row.names.data.frame(pedCsv_2),
    row.names.data.frame(nprcgenekeepr::pedGood)
  )
})
test_that("makeExamplePedigreeFile creates Excel files", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  files_csv <- saveDataframesAsFiles(dfList,
    baseDir = tempdir(),
    fileType = "excel"
  )
  pedCsv_1 <- suppressWarnings(getPedigree(files_csv[1L]))

  expect_named(pedCsv_1, names(nprcgenekeepr::lacy1989Ped))
  expect_identical(
    row.names.data.frame(pedCsv_1),
    row.names.data.frame(nprcgenekeepr::lacy1989Ped)
  )

  pedCsv_2 <- suppressWarnings(getPedigree(files_csv[2L]))
  expect_named(pedCsv_2, names(nprcgenekeepr::pedGood))
  expect_identical(
    row.names.data.frame(pedCsv_2),
    row.names.data.frame(nprcgenekeepr::pedGood)
  )
  # nolint end: object_name_linter.
})
