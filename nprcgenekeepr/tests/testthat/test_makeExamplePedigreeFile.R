#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("makeExamplePedigreeFile")
library(testthat)
test_that("makeExamplePedigreeFile creates file", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  pedigreeFile <- suppressMessages(makeExamplePedigreeFile())
  expect_true(all(file.exists(pedigreeFile)))
})
test_that("makeExamplePedigreeFile creates correct file contents", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  pedigreeFile <- suppressMessages(makeExamplePedigreeFile())
  pedCsv <- read.table(pedigreeFile,
    sep = ",", header = TRUE,
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(pedCsv), 3694L)
})
test_that("makeExamplePedigreeFile creates file", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  filePath <- file.path(tempdir(), "exampleFile.xlsx")
  if (file.exists(filePath)) {
    file.remove(filePath)
  }
  pedigreeFile <-
    suppressMessages(makeExamplePedigreeFile(
      file = filePath,
      fileType = "excel"
    ))
  expect_true(all(file.exists(pedigreeFile)))
  if (file.exists(filePath)) {
    file.remove(filePath)
  }
})
test_that("makeExamplePedigreeFile creates correct file contents", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  filePath <- file.path(tempdir(), "exampleFile.xlsx")
  if (file.exists(filePath)) {
    file.remove(filePath)
  }
  pedigreeFile <-
    suppressMessages(makeExamplePedigreeFile(
      file = filePath,
      fileType = "excel"
    ))
  pedExcel <- nprcgenekeepr:::readExcelPOSIXToCharacter(pedigreeFile)
  expect_identical(nrow(pedExcel), 3694L)
  if (file.exists(filePath)) {
    file.remove(filePath)
  }
})
test_that("makeExamplePedigreeFile creates file", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  pedigreeFile <-
    suppressMessages(makeExamplePedigreeFile(
      file = "exampleFile.txt",
      fileType = "txt"
    ))
  expect_true(all(file.exists(pedigreeFile)))
})
test_that("makeExamplePedigreeFile creates correct file contents", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  pedigreeFile <-
    suppressMessages(makeExamplePedigreeFile(
      file = "exampleFile.txt",
      fileType = "txt"
    ))
  pedTxt <- read.table(pedigreeFile,
    sep = ",", header = TRUE,
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(pedTxt), 3694L)
})
