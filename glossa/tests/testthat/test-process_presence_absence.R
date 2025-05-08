# Clean and processing of presence/absence points

# Create a fake dataset for testing
toy_data <- data.frame(
  decimalLongitude = c(-122.431297, NA, -122.431297, -122.431300),
  decimalLatitude = c(37.773972, 37.773972, 37.773972, 37.773970),
  timestamp = c(1, 1, 1, 2)
)

# Create a fake study area as an sf polygon
study_area_coords <- matrix(c(-123, 36, -121, 36, -121, 38, -123, 38, -123, 36), ncol = 2, byrow = TRUE)
study_area_polygon <- sf::st_polygon(list(study_area_coords))
study_area <- sf::st_sfc(study_area_polygon, crs = 4326) # EPSG:4326

test_that("clean_coordinates returns a data frame", {
  cleaned_data <- clean_coordinates(toy_data, study_area)
  expect_s3_class(cleaned_data, "data.frame")
})

test_that("clean_coordinates removes rows with NA coordinates", {
  cleaned_data <- clean_coordinates(toy_data, study_area)
  expect_true(all(!is.na(cleaned_data$decimalLongitude)))
})

test_that("clean_coordinates removes duplicate points", {
  cleaned_data <- clean_coordinates(toy_data, study_area)
  expect_equal(nrow(cleaned_data), 2) # After cleaning, we should have 2 unique points
})

test_that("clean_coordinates removes close points based on decimal_digits", {
  cleaned_data <- clean_coordinates(toy_data, study_area, decimal_digits = 0)
  expect_equal(nrow(cleaned_data), 2) # After cleaning, we should have 1 point left
})

test_that("clean_coordinates removes points outside the study area", {
  outside_data <- data.frame(
    decimalLongitude = c(-124.431297),
    decimalLatitude = c(39.773972),
    timestamp = c(1)
  )
  cleaned_data <- clean_coordinates(outside_data, study_area)
  expect_equal(nrow(cleaned_data), 0) # Should be empty since it's outside the area
})

test_that("clean_coordinates removes points overlapping the area", {
  cleaned_data <- clean_coordinates(toy_data, study_area, overlapping = TRUE)
  expect_equal(nrow(cleaned_data), 0) # Should keep overlapping points
})

test_that("clean_coordinates cleans based on timestamp", {
  timestamp_data <- data.frame(
    decimalLongitude = c(-122.431297, -122.431297),
    decimalLatitude = c(37.773972, 37.773972),
    timestamp = c(1, 2)
  )
  cleaned_data <- clean_coordinates(timestamp_data, study_area, by_timestamp = TRUE)
  expect_equal(nrow(cleaned_data), 2) # Should keep both since timestamps are different
})

test_that("clean_coordinates is reproducible with seed", {
  set.seed(123)
  cleaned_data1 <- clean_coordinates(toy_data, study_area, decimal_digits = 1, seed = 123)

  set.seed(123)
  cleaned_data2 <- clean_coordinates(toy_data, study_area, decimal_digits = 1, seed = 123)

  expect_equal(cleaned_data1, cleaned_data2) # Results should be the same
})

test_that("remove_duplicate_points wrongly formated input", {
  expect_error(remove_duplicate_points(df = matrix(1:9, nrow = 3)), "must be a data frame")
  expect_error(remove_duplicate_points(df = data.frame(lat = 1:3, long = 1:3), coords = 1), "character vector")
  expect_error(remove_duplicate_points(df = data.frame(lat = 1:3, long = 1:3), coords = c("a", "b")), "not found")
})

test_that("remove_points_polygon wrongly formated input", {
  expect_error(remove_points_polygon(df = matrix(1:9, nrow = 3)), "must be a data frame")
  expect_error(remove_points_polygon(df = data.frame(lat = 1:3, long = 1:3), coords = 1), "character vector")
  expect_error(remove_points_polygon(df = data.frame(lat = 1:3, long = 1:3), coords = c("a", "b")), "not found")
})

test_that("clean_coordinates without timestamp", {
  expect_s3_class(clean_coordinates(df = data.frame(lat = 1:3, long = 1:3), coords = c("lat", "long"), by_timestamp = FALSE, study_area = NULL)
, "data.frame")
})


