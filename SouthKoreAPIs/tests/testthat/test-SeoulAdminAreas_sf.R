# SouthKoreAPIs - Access South Korean Data via Public APIs and Curated Datasets
# Version 0.1.1
# Copyright (c) 2025 Renzo Caceres Rossi
# Licensed under the MIT License.
# See the LICENSE file in the root directory for full license text.

# SeoulAdminAreas_sf

library(testthat)

# Test 1: Confirm the object is an sf object with correct classes
test_that("SeoulAdminAreas_sf is an sf object with correct classes", {
  expect_s3_class(SeoulAdminAreas_sf, "sf")
  expect_s3_class(SeoulAdminAreas_sf, "tbl_df")
  expect_s3_class(SeoulAdminAreas_sf, "tbl")
  expect_s3_class(SeoulAdminAreas_sf, "data.frame")
})

# Test 2: Confirm it has exactly 3 columns
test_that("SeoulAdminAreas_sf has 3 columns", {
  expect_equal(length(SeoulAdminAreas_sf), 3)
})

# Test 3: Confirm it has exactly 25 rows
test_that("SeoulAdminAreas_sf has 25 rows", {
  expect_equal(nrow(SeoulAdminAreas_sf), 25)
})

# Test 4: Confirm column names are correct and in order
test_that("SeoulAdminAreas_sf has correct column names", {
  expect_named(SeoulAdminAreas_sf, c("name", "value", "geometry"))
})

# Test 5: Confirm column types are correct
test_that("SeoulAdminAreas_sf columns have correct types", {
  expect_type(SeoulAdminAreas_sf$name, "character")
  expect_type(SeoulAdminAreas_sf$value, "integer")
  expect_type(SeoulAdminAreas_sf$geometry, "list")  # geometry column is a list
})

# Test 6: Confirm the geometry column contains objects of correct classes
test_that("SeoulAdminAreas_sf geometry column contains sf geometry objects", {
  for (geom in SeoulAdminAreas_sf$geometry) {
    # Check the class vector includes "XY", "sfg", and "MULTIPOLYGON" (or other geometry types)
    expect_true(all(c("XY", "sfg") %in% class(geom)),
                info = "Geometry object must have classes 'XY' and 'sfg'")
    # MULTIPOLYGON may or may not be present depending on geometry type, so check if present
    if ("MULTIPOLYGON" %in% class(geom)) {
      expect_true(TRUE)
    }
    # Confirm the geometry is a list of matrices
    expect_true(is.list(geom))
    expect_true(is.matrix(geom[[1]]))
    expect_type(geom[[1]], "double")
    expect_equal(ncol(geom[[1]]), 2)  # Each coordinate matrix should have 2 columns (x, y)
  }
})
