library(arealDB)
library(ontologics)
library(testthat)
library(checkmate)
library(tibble)
context("matchOntology")


test_that("ontology is correct after geometry normalisation", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "regTable", path = dbpath)

  # normalise first level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al1__gadm.gpkg"))

  # use adb_ontology here instead
  onto <- load_ontology(path = paste0(dbpath, "/_meta/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "county_2", "municipality2_1", "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  # normalise second level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al2__gadm.gpkg"))

  onto <- load_ontology(path = paste0(dbpath, "/_meta/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "county_2", "municipality2_1", "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3", "gadm_2.3", NA, NA, "gadm_3.3", NA, "gadm_4.3", NA, "gadm_5.3", NA, NA))

  # normalise third level ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al3__gadm.gpkg"))

  onto <- load_ontology(path = paste0(dbpath, "/_meta/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "county_2", "municipality2_1", "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3", "gadm_2.3", "gadm_6.3", "gadm_7.3", "gadm_3.3", "gadm_8.3", "gadm_4.3", "gadm_9.3", "gadm_5.3", "gadm_10.3", "gadm_11.3"))

  # normalise a non-gadm dataset that has been attached to the DB ----
  output <- normGeometry(input = paste0(getOption("adb_path"), "/geometries/stage2/_al3__madeUp.gpkg"))

  onto <- load_ontology(path = paste0(dbpath, "/_meta/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "Gemeinde 13", "Gemeinde 14", "county_2", "municipality2_1", "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3 | madeUp_1.3", "gadm_2.3", "gadm_6.3", "gadm_7.3", "madeUp_2.3", "madeUp_3.3", "gadm_3.3", "gadm_8.3 | madeUp_4.3", "gadm_4.3", "gadm_9.3 | madeUp_5.3", "gadm_5.3", "gadm_10.3 | madeUp_6.3", "gadm_11.3 | madeUp_7.3"))
  expect_set_equal(x = onto@concepts$harmonised$has_broader_match,
                   y = c(NA, NA, "madeUp_2.3", "madeUp_2.3", NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_set_equal(x = onto@concepts$harmonised$has_narrower_match,
                   y = c(NA, NA, NA, "madeUp_3.3", NA, NA, NA, NA, NA, NA, NA, NA, NA))

})

test_that("ontology is correct after table normalisation", {

  dbpath <- paste0(tempdir(), "/newDB")

  adb_example(until = "normGeometry", path = dbpath)

  output <- normTable()

  onto <- load_ontology(path = paste0(dbpath, "/_meta/territories.rds"))
  expect_set_equal(x = onto@concepts$harmonised$label,
                   y = c("a_nation", "county_1", "municipality1_1", "municipality1_2", "Gemeinde 13", "Gemeinde 14", "county_2", "municipality2_1", "county_3", "municipality3", "county_4", "municipality4_1", "municipality4_2"))
  expect_set_equal(x = onto@concepts$harmonised$has_close_match,
                   y = c("gadm_1.3 | madeUp_1.3", "gadm_2.3 | madeUp_8.3", "gadm_6.3", "gadm_7.3", "madeUp_2.3", "madeUp_3.3", "gadm_3.3", "gadm_8.3 | madeUp_4.3", "gadm_4.3 | madeUp_9.3", "gadm_9.3 | madeUp_5.3", "gadm_5.3 | madeUp_10.3", "gadm_10.3 | madeUp_6.3", "gadm_11.3 | madeUp_7.3"))
  expect_set_equal(x = onto@concepts$harmonised$has_broader_match,
                   y = c(NA, NA, "madeUp_2.3", "madeUp_2.3", NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_set_equal(x = onto@concepts$harmonised$has_narrower_match,
                   y = c(NA, NA, NA, "madeUp_3.3", NA, NA, NA, NA, NA, NA, NA, NA, NA))
})
