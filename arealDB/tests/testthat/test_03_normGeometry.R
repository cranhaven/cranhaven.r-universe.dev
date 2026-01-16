library(arealDB)
library(ontologics)
library(sf)
library(testthat)
library(checkmate)
context("normGeometry")

test_that("geometries can be normalised", {

  dbpath <- paste0(tempdir(), "/newDB")
  adb_example(until = "regTable", path = dbpath)

  inPath <- system.file("test_datasets", package = "arealDB", mustWork = TRUE)

  # normalise first level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al1__gadm.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/geometries/stage2/processed/_al1__gadm.gpkg"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/geometries/stage3/a_nation.gpkg"), layer = "al1", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 1, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))

  # normalise second level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al2__gadm.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/geometries/stage2/processed/_al2__gadm.gpkg"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/geometries/stage3/a_nation.gpkg"), layer = "al2", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 4, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))

  # normalise third level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al3__gadm.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))
  expect_file_exists(x = paste0(getOption("adb_path"), "/geometries/stage2/processed/_al3__gadm.gpkg"))

  # normalise a non-gadm dataset that has been attached to the DB ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al3__madeUp.gpkg"))
  expect_tibble(x = output, nrows = 1, ncols = 11, col.names = "strict")
  expect_names(x = names(output), must.include = c("geoID", "datID", "stage2_name", "layer", "label", "ancillary", "stage1_name", "stage1_url", "download_date", "update_frequency", "notes"))

  final <- st_read(dsn = paste0(getOption("adb_path"), "/geometries/stage3/a_nation.gpkg"), layer = "al3", quiet = TRUE)
  expect_class(x = final, classes = c("sf"))
  expect_data_frame(x = final, nrows = 12, ncols = 7)
  expect_names(x = names(final), identical.to = c("gazID", "gazName", "gazClass", "match", "external", "geoID", "geom"))
  expect_names(x = final$gazID, identical.to = c(".001.001.001", ".001.001.002", ".001.001.003", ".001.001.004", ".001.002.001", ".001.002.001", ".001.003.001", ".001.003.001", ".001.004.001", ".001.004.001", ".001.004.002", ".001.004.002"))
  expect_names(x = final$match, identical.to = c("close", "close", "broader [83<>100_.001.001.001] | broader [17<>20_.001.001.002]", "narrower [100<>80_.001.001.002]", "close", "close [100<>100_.001.002.001]", "close", "close [100<>100_.001.003.001]", "close", "close [100<>100_.001.004.001]", "close", "close [100<>100_.001.004.002]"))

})
