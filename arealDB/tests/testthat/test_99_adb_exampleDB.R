library(arealDB)
library(testthat)
library(checkmate)
library(readr)
context("adb_example")


test_that("make example DB until start_arealDB", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "adb_init", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/_meta/inventory.rds"))
  assert_directory_exists(x = paste0(dbpath, "/geometries"))
  assert_directory_exists(x = paste0(dbpath, "/tables"))
  assert_directory_exists(x = paste0(dbpath, "/_meta/documentation"))

  assert_file_exists(x = paste0(dbpath, "/geometries/stage1/example_geom.7z"))
  assert_file_exists(x = paste0(dbpath, "/tables/stage1/madeUp/example_table.7z"))
  assert_file_exists(x = paste0(dbpath, "/_meta/schemas/example_schema.rds"))

})

test_that("make example DB until regDataseries", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "regDataseries", path = dbpath)

  datID <- readRDS(file = paste0(dbpath, "/_meta/inventory.rds"))$dataseries
  expect_true(object = all(dim(datID) == c(2, 7)))

})

test_that("make example DB until regGeometry", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "regGeometry", path = dbpath)

  geoID <- readRDS(file = paste0(dbpath, "/_meta/inventory.rds"))$geometries
  expect_true(object = all(dim(geoID) == c(4, 11)))

  expect_file_exists(x = paste0(dbpath, "/geometries/stage2/_al1__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/geometries/stage2/_al2__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/geometries/stage2/_al3__gadm.gpkg"))
  expect_file_exists(x = paste0(dbpath, "/geometries/stage2/_al3__madeUp.gpkg"))

})

test_that("make example DB until regTable", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "regTable", path = dbpath)

  geoID <- readRDS(file = paste0(dbpath, "/_meta/inventory.rds"))$tables
  expect_true(object = all(dim(geoID) == c(2, 16)))

  expect_file_exists(x = paste0(dbpath, "/tables/stage2/_al1_barleyMaize_1990_2017_madeUp.csv"))
  expect_file_exists(x = paste0(dbpath, "/tables/stage2/aNation_al2_barleyMaize_1990_2017_madeUp.csv"))

})

test_that("make example DB until normGeometry", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normGeometry", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/geometries/stage3/a_nation.gpkg"))

})

test_that("make example DB until normTable", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normTable", path = dbpath)

  expect_file_exists(x = paste0(dbpath, "/tables/stage3/a_nation.rds"))

})


