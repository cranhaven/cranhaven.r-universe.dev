# Process environmental layers

test_that("Mask envrionmental layers with study area", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "fit_layers.zip", package="glossa")
  file2 <- system.file("extdata", "world.gpkg", package="glossa")
  layers <- suppressWarnings(glossa::read_layers_zip(file_path = file1))
  study_area <- glossa::read_extent_polygon(file_path = file2)

  expect_s4_class(glossa::layer_mask(layers[[1]], study_area), "SpatRaster")
})

# Create a toy SpatRaster for testing
raster_data <- matrix(1:100, nrow = 10)  # A 10x10 matrix
toy_raster <- terra::rast(raster_data, ext = c(-123, -121, 36, 38), crs = "epsg:4326")  # Create a SpatRaster

# Create a toy polygon for testing
polygon_coords <- matrix(c(-123, 36, -121, 36, -121, 38, -123, 38, -123, 36), ncol = 2, byrow = TRUE)
toy_polygon <- sf::st_polygon(list(polygon_coords))
toy_sf_polygon <- sf::st_sfc(toy_polygon, crs = 4326)  # Create an sf object

# Tests for layer_mask
test_that("layer_mask returns a SpatRaster object", {
  masked_layers <- layer_mask(toy_raster, toy_sf_polygon)
  expect_s4_class(masked_layers, "SpatRaster")  # Check that the result is a SpatRaster
})

test_that("layer_mask throws error for non-SpatRaster input", {
  expect_error(layer_mask(NULL, toy_sf_polygon), "Argument 'layers' must be a 'SpatRaster' object.")
})

test_that("layer_mask throws error for non-sf polygon input", {
  skip_if_not_installed("terra")
  non_sf_polygon <- terra::vect(matrix(c(-123, 36, -121, 36, -121, 38, -123, 38, -123, 36), ncol = 2, byrow = TRUE), type = "polygons")
  expect_error(layer_mask(toy_raster, non_sf_polygon), "Argument 'study_area' must be an 'sf' object.")
})

test_that("layer_mask processes raster correctly within polygon", {
  skip_if_not_installed("terra")
  masked_layers <- layer_mask(toy_raster, toy_sf_polygon)

  # Check the number of layers remains the same
  expect_equal(terra::nlyr(masked_layers), 1)  # Should have 1 layer

  # Check if the extent of masked layers is within the polygon extent
  masked_extent <- terra::ext(masked_layers)
  expect_true(all(masked_extent[1:2] >= -123 & masked_extent[3:4] <= 38))  # Should be within the polygon bounds
})
