library(arealDB)
library(testthat)
library(checkmate)
context("adb_translations")


test_that("", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normTable", path = dbpath)

  match <- adb_translations(type = "gazetteer", dataseries = "madeUp")

  expect_data_frame(x = match, nrows = 11, ncols = 9)
  expect_names(x = names(match), permutation.of = c("label", "class", "id", "has_broader", "description", "has_broader_match", "has_close_match", "has_exact_match", "has_narrower_match"))

})