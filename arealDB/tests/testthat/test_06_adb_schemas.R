library(arealDB)
library(testthat)
library(checkmate)
context("adb_schemas")


test_that("", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normTable", path = dbpath)

  schemas <- adb_schemas(pattern = "madeUp")

  expect_list(x = schemas, len = 2, types = "schema")
  expect_names(x = names(schemas), identical.to = c("_al1_barleyMaize_1990_2017_madeUp_schema", "aNation_al2_barleyMaize_1990_2017_madeUp_schema"))

})
