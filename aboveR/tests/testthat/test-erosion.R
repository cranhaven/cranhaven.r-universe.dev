# Tests for detect_channels() and pond_sedimentation()

dem_before_path <- system.file("extdata/dem_before.tif", package = "aboveR")
dem_after_path  <- system.file("extdata/dem_after.tif", package = "aboveR")
boundary_path   <- system.file("extdata/boundary.gpkg", package = "aboveR")

test_that("detect_channels() returns raster by default", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_before_path)

  channels <- detect_channels(dem, threshold = 50)

  expect_s4_class(channels, "SpatRaster")
  expect_equal(names(channels), "channel")
})

test_that("detect_channels() validates input", {
  expect_error(detect_channels("not_raster"), "SpatRaster")
})

test_that("pond_sedimentation() returns list with volume", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  before   <- terra::rast(dem_before_path)
  after    <- terra::rast(dem_after_path)
  boundary <- sf::st_read(boundary_path, quiet = TRUE)

  sed <- pond_sedimentation(before, after, boundary)

  expect_type(sed, "list")
  expect_true(all(c("sediment_volume_m3", "mean_depth_change_m",
                     "pond_area_m2", "change_raster") %in% names(sed)))
  expect_s4_class(sed$change_raster, "SpatRaster")
  expect_true(sed$sediment_volume_m3 >= 0)
})
