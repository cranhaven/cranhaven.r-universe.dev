
test_that("Get covariate names from zip", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "project_layers_1.zip", package="glossa")


  expect_type(glossa::get_covariate_names(file1), "character")
})

test_that("extract_noNA_cov_values handles NA values correctly", {
  # Create sample occurrence data
  occ_data <- data.frame(
    decimalLongitude = c(-120, -121, -1280, -122),
    decimalLatitude = c(35, 36, 37, 38),
    timestamp = c(1, 1, 1, 2)
  )

  # Create toy raster layers
  raster1 <- terra::rast(nrows = 10, ncols = 10, ext = c(-125, -115, 30, 40), vals = runif(100))
  raster2 <- terra::rast(nrows = 10, ncols = 10, ext = c(-125, -115, 30, 40), vals = runif(100))
  covariate_layers <- list(raster1, raster2)
  names(covariate_layers) <- c("time1", "time2")

  # Extract covariate values
  result <- extract_noNA_cov_values(occ_data, covariate_layers, "lyr.1")

  # Check that only valid rows are present (2 valid occurrences)
  expect_equal(nrow(result), 3)
  expect_true("lyr.1" %in% colnames(result))
  expect_false(any(is.na(result)))
})

test_that("create_coords_layer generates correct coordinate layers", {
  # Create a toy raster layer
  toy_raster <- terra::rast(nrows = 10, ncols = 10, ext = c(-125, -115, 30, 40), vals = runif(100), crs = "epsg:4326")

  # Generate coordinate layers without scaling or masking
  coords_layer <- create_coords_layer(toy_raster, scale_layers = FALSE)

  # Check if the correct number of layers (longitude and latitude) are created
  expect_equal(terra::nlyr(coords_layer), 2)  # Expect 2 layers (lon, lat)

  # Check if coordinate values are within expected range
  long_vals <- terra::values(coords_layer[[1]])
  lat_vals <- terra::values(coords_layer[[2]])
  expect_true(all(long_vals >= -125 & long_vals <= -115))
  expect_true(all(lat_vals >= 30 & lat_vals <= 40))
})
