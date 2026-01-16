library(testthat)
library(checkmate)
context("adb_init")

test_that("path has been added to the global options", {

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)
  dbpath <- paste0(tempdir(), "/newDB")
  gazPath <- paste0(dbpath, "/territories.rds")

  dir.create(file.path(dbpath))
  saveRDS(object = arealDB::territories, file = gazPath)
  adb_init(root = dbpath,
           version = "some0.0.1", licence = "https://creativecommons.org/licenses/by-sa/4.0/",
           author = list(cre = "Gordon Freeman", aut = "Alyx Vance", ctb = "The G-Man"),
           gazetteer = gazPath, top = "al1",
           ontology = list(commodity = paste0(inPath, "/ontology.rds")))

  out <- getOption("adb_path")
  expect_character(x = out)
  expect_true(out == dbpath)
})

test_that("Error if arguments have wrong value", {
  expect_error(adb_init(root = 1))
})
