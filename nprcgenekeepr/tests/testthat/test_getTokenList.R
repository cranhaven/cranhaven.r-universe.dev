#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getTokenList")
library(testthat)
lines <- c(
  "center = \"SNPRC\"",
  " baseUrl = \"https://boomer.txbiomed.local:8080/labkey\"",
  " schemaName = \"study\"", " folderPath = \"/SNPRC\"",
  " queryName = \"demographics\"",
  "lkPedColumns = (\"Id\", \"gender\", \"birth\", \"death\",",
  "              \"lastDayAtCenter\", \"dam\", \"sire\")",
  "mapPedColumns = (\"id\", \"sex\", \"birth\", \"death\", ",
  "  \"exit\", \"dam\", \"sire\")"
)
lkVec <- c(
  "Id", "gender", "birth", "death",
  "lastDayAtCenter", "dam", "sire"
)
mapVec <- c("id", "sex", "birth", "death", "exit", "dam", "sire")

test_that("getTokenList returns correct lines and vectors", {
  tokenList <- getTokenList(lines)
  params <- tokenList$param
  tokenVectors <- tokenList$tokenVec
  expect_equal(params, c(
    "center", "baseUrl", "schemaName", "folderPath",
    "queryName", "lkPedColumns", "mapPedColumns"
  ))
  expect_identical(tokenVectors[[1L]], "SNPRC")
  expect_identical(tokenVectors[[2L]], "https://boomer.txbiomed.local:8080/labkey")
  expect_identical(tokenVectors[[3L]], "study")
  expect_identical(tokenVectors[[4L]], "/SNPRC")
  expect_identical(tokenVectors[[5L]], "demographics")
  expect_identical(tokenVectors[[6L]], lkVec)
  expect_identical(tokenVectors[[7L]], mapVec)
})
