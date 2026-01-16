library(arealDB)
library(testthat)
library(tabshiftr)
library(readr)
library(checkmate)
library(ontologics)
context("normTable")


test_that("tables can be normalised (without matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  output <- normTable(input = paste0(getOption("adb_path"), "/tables/stage2/_al1_barleyMaize_1990_2017_madeUp.csv"))


  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/tables/stage2/processed/_al1_barleyMaize_1990_2017_madeUp.csv"))
  final <- readRDS(file = paste0(getOption("adb_path"), "/tables/stage3/a_nation.rds"))
  expect_tibble(x = final, types = c("integer", "integer", "integer", "character", "integer", "character", "double", "double"))
  expect_data_frame(x = final, nrows = 56, ncols = 9)
  expect_names(x = names(final), identical.to = c("tabID", "geoID", "gazID", "gazName", "gazMatch", "year", "commodity", "harvested", "production"))

})

test_that("tables can be normalised (with matched variables)", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "normGeometry", path = dbpath)

  output <- normTable()

  # test whether the resulting file is "correct" ----
  expect_file_exists(x = paste0(getOption("adb_path"), "/tables/stage2/processed/_al1_barleyMaize_1990_2017_madeUp.csv"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/tables/stage2/processed/aNation_al2_barleyMaize_1990_2017_madeUp.csv"))
  final <- readRDS(file = paste0(getOption("adb_path"), "/tables/stage3/a_nation.rds"))
  expect_data_frame(x = final, nrows = 280, ncols = 9)
  expect_names(x = names(final), identical.to = c("tabID", "geoID", "gazID", "gazName", "gazMatch", "year", "commodity", "harvested", "production"))

})
