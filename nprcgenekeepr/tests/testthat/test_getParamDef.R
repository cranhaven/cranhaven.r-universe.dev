#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getParamDef")
library(testthat)
tokens <- structure(list(
  param = c(
    "center", "baseUrl", "schemaName", "folderPath", "queryName",
    "lkPedColumns", "mapPedColumns", "sqlServerDSN", "sqlServerPed",
    "cd.at_sfbr", "m.id"
  ),
  tokenVec = list(
    "SNPRC", "http://vger/labkey", "study", "/SNPRC", "demographics",
    c("Id", "gender", "birth", "death", "lastDayAtCenter", "dam", "sire"),
    c("id", "sex", "birth", "death", "exit", "dam", "sire"),
    "frogstar-vortex-animal-msharp",
    c(
      "select", "m.id", "m.sex", "convertchar12", "m.birth_date", "110",
      "as", "birth", "convertchar12", "m.death_date", "110", "as", "death",
      "CASE", "WHEN"
    ),
    c(
      "'Y'", "THEN", "NULL", "ELSE", "cd.disp_date_tm_max", "END", "AS",
      "lastDayAtCenter", "m.dam_id", "as", "dam", "m.sire_id", "as", "sire",
      "from", "master", "m", "inner", "join", "current_data", "cd", "on"
    ),
    c("cd.id", "where", "m.id", "in", "'", "ids_str", "'")
  )
), .Names = c("param", "tokenVec"))
test_that("getParamDef returns the correct values", {
  expect_identical(getParamDef(tokens, "baseUrl"), "http://vger/labkey")
  expect_identical(getParamDef(tokens, "center"), "SNPRC")
  expect_identical(getParamDef(tokens, "queryName"), "demographics")
  expect_error(getParamDef(tokens, "thisIsNotAParam"))
})
