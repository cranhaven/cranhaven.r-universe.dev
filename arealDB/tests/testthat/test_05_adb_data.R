library(arealDB)
context("adb_data")


test_that("", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normTable", path = dbpath)

  # adb_data()



})