# Invert polygon

test_that("Invert polygon", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "world.gpkg", package="glossa")
  study_area <- glossa::read_extent_polygon(file_path = file1)

  expect_s3_class(glossa::invert_polygon(study_area), "sfc")

  expect_s3_class(glossa::invert_polygon(study_area, bbox = c(xmin = -180, ymin = -90, xmax = 180, ymax = 90)), "sfc")
})


# Create a toy polygon for testing
polygon_coords <- matrix(c(-123, 36, -121, 36, -121, 38, -123, 38, -123, 36), ncol = 2, byrow = TRUE)
toy_polygon <- sf::st_polygon(list(polygon_coords))
toy_sf_polygon <- sf::st_sfc(toy_polygon, crs = 4326)

# Tests for buffer_polygon
test_that("buffer_polygon returns a buffered polygon", {
  buffered <- suppressWarnings(buffer_polygon(toy_sf_polygon, buffer_distance = 0.1))
  expect_s3_class(buffered, "sfc_POLYGON") # Check that the result is an sfc_POLYGON
})

test_that("buffer_polygon throws error for non-sf input", {
  expect_error(buffer_polygon(NULL, 0.1), "Input must be an 'sf' object representing a polygon.")
})

test_that("buffer_polygon throws error for non-numeric buffer distance", {
  expect_error(buffer_polygon(toy_sf_polygon, "string"), "Buffer distance must be a single numeric value.")
})

test_that("buffer_polygon throws error for buffer distance of length not equal to 1", {
  expect_error(buffer_polygon(toy_sf_polygon, c(0.1, 0.2)), "Buffer distance must be a single numeric value.")
})

# Tests for invert_polygon
test_that("invert_polygon returns an inverted polygon", {
  inverted <- invert_polygon(toy_sf_polygon)
  expect_s3_class(inverted, "sfc") # Check that the result is an sfc_POLYGON
})

test_that("invert_polygon throws error for non-sf input", {
  expect_error(invert_polygon(NULL), "Input must be an 'sf' object representing a polygon.")
})

test_that("invert_polygon handles provided bbox correctly", {
  custom_bbox <- sf::st_sfc(sf::st_polygon(list(matrix(c(-124, 35, -120, 35, -120, 39, -124, 39, -124, 35), ncol = 2, byrow = TRUE))), crs = 4326)
  inverted <- invert_polygon(toy_sf_polygon, bbox = custom_bbox)
  expect_s3_class(inverted, "sfc_POLYGON") # Check that the result is an sfc_POLYGON
})
