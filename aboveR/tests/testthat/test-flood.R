# Tests for flood_inundation(), flood_depth(), height_above_drainage()

dem_path <- system.file("extdata/dem_before.tif", package = "aboveR")
boundary_path <- system.file("extdata/boundary.gpkg", package = "aboveR")

test_that("flood_inundation() returns binary raster", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- flood_inundation(dem, water_level = 305)

  expect_s4_class(result, "SpatRaster")
  expect_equal(names(result), "inundated")
  vals <- terra::values(result)[, 1]
  vals <- vals[!is.na(vals)]
  expect_true(all(vals == 1))
})

test_that("flood_inundation() respects boundary", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)
  bnd <- sf::st_read(boundary_path, quiet = TRUE)

  result <- flood_inundation(dem, water_level = 305, boundary = bnd)
  expect_s4_class(result, "SpatRaster")
})

test_that("flood_inundation() validates water_level", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  expect_error(flood_inundation(dem, water_level = "high"), "numeric")
  expect_error(flood_inundation(dem, water_level = c(300, 305)), "single")
})

test_that("flood_depth() returns depth raster", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- flood_depth(dem, water_level = 305)

  expect_s4_class(result, "SpatRaster")
  expect_equal(names(result), "flood_depth")

  vals <- terra::values(result)[, 1]
  vals <- vals[!is.na(vals)]
  expect_true(all(vals > 0))
})

test_that("flood_depth() returns NA above water level", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)
  dem_vals <- terra::values(dem)[, 1]

  # Set water level below most of the terrain
  low_level <- min(dem_vals, na.rm = TRUE) + 1

  result <- flood_depth(dem, water_level = low_level)
  vals <- terra::values(result)[, 1]

  # Most cells should be NA (above water)
  expect_true(sum(is.na(vals)) > sum(!is.na(vals)))
})

test_that("height_above_drainage() returns HAND raster", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  result <- height_above_drainage(dem, window = 7)

  expect_s4_class(result, "SpatRaster")
  expect_equal(names(result), "hand")

  vals <- terra::values(result)[, 1]
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= 0))
})

test_that("height_above_drainage() validates window", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  expect_error(height_above_drainage(dem, window = 6), "odd")
})
