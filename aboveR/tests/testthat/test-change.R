# Tests for terrain_change() and change_by_zone()

dem_before_path <- system.file("extdata/dem_before.tif", package = "aboveR")
dem_after_path  <- system.file("extdata/dem_after.tif", package = "aboveR")
zones_path      <- system.file("extdata/zones.gpkg", package = "aboveR")

test_that("terrain_change() returns two-layer SpatRaster", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  before <- terra::rast(dem_before_path)
  after  <- terra::rast(dem_after_path)

  result <- terrain_change(before, after)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 2)
  expect_true("change" %in% names(result))
  expect_true("class" %in% names(result))
})

test_that("terrain_change() detects cut and fill", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  before <- terra::rast(dem_before_path)
  after  <- terra::rast(dem_after_path)

  result <- terrain_change(before, after)
  change_vals <- terra::values(result[["change"]])[, 1]
  change_vals <- change_vals[!is.na(change_vals)]

  # Sample data has cuts (center) and fills (SE corner)
  expect_true(any(change_vals < -0.1))
  expect_true(any(change_vals > 0.1))
})

test_that("terrain_change() validates CRS match", {
  r1 <- terra::rast(nrows = 10, ncols = 10, crs = "EPSG:32617")
  terra::values(r1) <- 1:100
  r2 <- terra::rast(nrows = 10, ncols = 10, crs = "EPSG:4326")
  terra::values(r2) <- 1:100

  expect_error(terrain_change(r1, r2), "CRS mismatch")
})

test_that("terrain_change() rejects non-raster input", {
  expect_error(terrain_change("not_a_raster", "also_not"), "SpatRaster")
})

test_that("change_by_zone() returns sf with volume columns", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  before <- terra::rast(dem_before_path)
  after  <- terra::rast(dem_after_path)
  zones  <- sf::st_read(zones_path, quiet = TRUE)

  chg <- terrain_change(before, after)
  result <- change_by_zone(chg, zones, id_field = "zone_id")

  expect_s3_class(result, "sf")
  expect_true("cut_volume" %in% names(result))
  expect_true("fill_volume" %in% names(result))
  expect_true("net_volume" %in% names(result))
  expect_equal(nrow(result), 2)
})
