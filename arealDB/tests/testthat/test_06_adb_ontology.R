library(arealDB)
library(testthat)
library(checkmate)
context("adb_ontology")


test_that("", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normTable", path = dbpath)

  onto <- adb_ontology(type = "gazetteer")

  expect_data_frame(x = onto, nrows = 13, ncols = 9)
  expect_names(x = names(onto), identical.to = c("id", "label", "class", "has_broader", "description", "has_broader_match", "has_close_match", "has_exact_match", "has_narrower_match"))

})
