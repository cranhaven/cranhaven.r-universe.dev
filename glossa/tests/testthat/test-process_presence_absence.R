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
  cleaned_data1 <- clean_coordinates(toy_data, study_area, thinning_method = "precision", thinning_value = 1, seed = 123)

  set.seed(123)
  cleaned_data2 <- clean_coordinates(toy_data, study_area, thinning_method = "precision", thinning_value = 1, seed = 123)

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

test_that("Distance thinning reduces nearby points", {
  df <- data.frame(
    decimalLongitude = c(0, 0.001, 0.002),
    decimalLatitude = c(0, 0.001, 0.002),
    timestamp = 2000,
    pa = 1
  )
  thinned <- clean_coordinates(df, study_area = NULL, thinning_method = "distance", thinning_value = 500000)
  expect_lte(nrow(thinned), 1)
})

test_that("Grid thinning reduces to one per cell", {
  df <- expand.grid(
    decimalLongitude = seq(0, 0.1, 0.01),
    decimalLatitude = seq(0, 0.1, 0.01)
  )
  df$timestamp <- 2000
  df$pa <- 1
  thinned <- clean_coordinates(df, study_area = NULL, thinning_method = "grid", thinning_value = 0.05)
  expect_lt(nrow(thinned), nrow(df))
})

test_that("Precision thinning removes duplicate rounded points", {
  df <- data.frame(
    decimalLongitude = c(1.12345, 1.12349),
    decimalLatitude = c(2.98765, 2.98769),
    timestamp = 2000,
    pa = 1
  )
  thinned <- clean_coordinates(df, study_area = NULL, thinning_method = "precision", thinning_value = 3)
  expect_equal(nrow(thinned), 1)
})

test_that("No thinning returns all cleaned points", {
  df <- data.frame(
    decimalLongitude = c(1, 2, 3),
    decimalLatitude = c(1, 2, 3),
    timestamp = 2000,
    pa = 1
  )
  out <- clean_coordinates(df, study_area = NULL, thinning_method = NULL)
  expect_equal(nrow(out), 3)
})

test_that("Invalid thinning method throws warning", {
  df <- data.frame(
    decimalLongitude = c(1, 2, 3),
    decimalLatitude = c(1, 2, 3),
    timestamp = 2000,
    pa = 1
  )
  expect_warning(clean_coordinates(df, study_area = NULL, thinning_method = "Banana"), "Unknown thinning method")
})



