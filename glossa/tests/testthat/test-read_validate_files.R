# Read input data from user

test_that("Read presence/absence files", {
  # skip on CRAN
  skip_on_cran()

  # File paths
  file1 <- system.file("extdata", "sp1.txt", package="glossa")
  file2 <- system.file("extdata", "sp2.csv", package="glossa")

  expect_s3_class(glossa::read_presences_absences_csv(file_path = file1), "data.frame")

  expect_s3_class(glossa::read_presences_absences_csv(file_path = file2, file_name = "sp2" ), "data.frame")
})

test_that("Read fit_layers file", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "fit_layers.zip", package="glossa")

  suppressWarnings(
    expect_s4_class(glossa::read_layers_zip(file_path = file1)[[1]], "SpatRaster")
  )
})

test_that("Read projection layers", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "project_layers_1.zip", package="glossa")

  suppressWarnings(
    expect_type(glossa::read_layers_zip(file_path = file1), "list")
  )
})

test_that("Validate projection layers", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "fit_layers.zip", package="glossa")
  file2 <- system.file("extdata", "project_layers_1.zip", package="glossa")

  expect_type(suppressWarnings(glossa::validate_layers_zip(file_path = file1)), "logical")
  expect_equal(glossa::validate_fit_projection_layers(file1, file2), TRUE)
})

test_that("Read extent polygon", {
  # skip on CRAN
  skip_on_cran()

  # File path
  file1 <- system.file("extdata", "world.gpkg", package="glossa")

  suppressWarnings(
    expect_s3_class(glossa::read_extent_polygon(file_path = file1), "sfc")
  )
})

# Load toy data
pa_data <- system.file("extdata", "testthat_coords.csv", package="glossa")
fit_layers <- system.file("extdata", "testthat_layers.zip", package="glossa")
study_area <- system.file("extdata", "testthat_polygon.gpkg", package="glossa")

# Tests for read_presences_absences_csv
test_that("read_presences_absences_csv reads and validates CSV correctly", {
  data <- read_presences_absences_csv(pa_data, show_modal = FALSE)
  expect_s3_class(data, "data.frame")  # Expecting a data frame
  expect_equal(ncol(data), 5)  # Expecting 5 columns
  expect_true(all(c("decimalLongitude", "decimalLatitude", "timestamp_original" , "timestamp", "pa") %in% colnames(data)))  # Check required columns
})

test_that("read_presences_absences_csv throws error for missing columns", {
  skip_on_cran()

  testhtat_file <- tempfile()
  write.table(data.frame(decimalLongitude = c(-123.1)), testhtat_file, row.names = FALSE, sep = "\t")
  expect_null(suppressWarnings(read_presences_absences_csv(testhtat_file)))
  unlink(testhtat_file)
})

# Tests for read_layers_zip
test_that("read_layers_zip loads layers correctly", {
  layers <- read_layers_zip(fit_layers, extend = TRUE, first_layer = TRUE)
  expect_true(is.list(layers))  # Expecting a list of raster layers
})

test_that("read_layers_zip loads layers correctly", {
  layers <- read_layers_zip(fit_layers, extend = FALSE, first_layer = FALSE)
  expect_true(is.list(layers))  # Expecting a list of raster layers
})

# Tests for read_extent_polygon
test_that("read_extent_polygon reads and validates polygon correctly", {
  extent_polygon <- read_extent_polygon(study_area)
  expect_s3_class(extent_polygon, "sfc")  # Expecting an sfc object
})

test_that("read_extent_polygon throws error for invalid geometry", {
  skip_on_cran()

  testhtat_file <- tempfile()
  writeLines("INVALID GEOMETRY", testhtat_file)
  expect_null(suppressWarnings(read_extent_polygon(testhtat_file)))
  unlink(testhtat_file)
})

# Tests for validate_layers_zip
test_that("validate_layers_zip validates layers correctly", {
  result <- suppressWarnings(validate_layers_zip(fit_layers))
  expect_true(result)  # Expecting TRUE for valid layers
})

# Tests for validate_fit_projection_layers
test_that("validate_fit_projection_layers checks covariates correctly", {
  result <- validate_fit_projection_layers(fit_layers, fit_layers)
  expect_true(result)  # Expecting TRUE for matching covariates
})

# Tests for validate_pa_fit_time
test_that("validate_pa_fit_time checks timestamps correctly", {
  result <- suppressWarnings(validate_pa_fit_time(read_presences_absences_csv(pa_data), fit_layers))
  expect_false(result)  # Expecting FALSE if the timestamps don't match
})

# Test for raster_timestamo_file
test_that("Timestamp mapping aligns rasters and occurrences correctly", {
  # Simulate raster layers named like raster_1950.tif, ..., raster_1954.tif
  years <- 10:20
  raster_list <- lapply(years, function(y) {
    r1 <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
    terra::values(r1) <- y  # the value is equal to the year
    names(r1) <- "var1"

    r2 <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
    terra::values(r2) <- y  # the value is equal to the year
    names(r2) <- "var2"

    c(r1, r2)
  })

  # Simulate timestamp mapping
  timestamp_file <- tempfile(fileext = ".txt")
  for (i in years) {
    cat(i, "\n", file = timestamp_file, append = TRUE)
  }

  # Simulate occurrences for sp1
  set.seed(123)
  years_sp1 <- 12:16
  sp1_df <- data.frame(
    decimalLongitude = runif(5, 0, 1),
    decimalLatitude = runif(5, 0, 1),
    timestamp = years_sp1,
    pa = sample(c(0, 1), 5, replace = TRUE)
  )
  sp1_file <- tempfile(fileext = ".tsv")
  write.table(sp1_df, file = sp1_file, col.names = TRUE, sep = "\t", dec = ".", row.names = FALSE, quote = FALSE)

  years_sp2 <- c(11, 15, 18, 20, 20)
  sp2_df <- data.frame(
    decimalLongitude = runif(5, 0, 1),
    decimalLatitude = runif(5, 0, 1),
    timestamp = years_sp2,
    pa = sample(c(0, 1), 5, replace = TRUE)
  )
  sp2_file <- tempfile(fileext = ".tsv")
  write.table(sp2_df, file = sp2_file, col.names = TRUE, sep = "\t", dec = ".", row.names = FALSE, quote = FALSE)

  # Read files
  raster_timestamp <- read.table(timestamp_file, header = FALSE, stringsAsFactors = FALSE)[, 1]
  pa_data <- list(
    "sp1" = read_presences_absences_csv(sp1_file, timestamp_mapping = raster_timestamp),
    "sp2" = read_presences_absences_csv(sp2_file, timestamp_mapping = raster_timestamp),
    "sp1_wrong" = read_presences_absences_csv(sp1_file),
    "sp2_wrong" = read_presences_absences_csv(sp2_file)
  )


  # Extract values for building model matrix
  res <- lapply(pa_data, function(x){
    extract_noNA_cov_values(x, raster_list, c("var1", "var2"))
  })

  # Check extracted matches years
  expect_equal(res$sp1$var1, years_sp1)
  expect_equal(res$sp2$var1, years_sp2)
  expect_true(any(res$sp1_wrong$var1 != years_sp1))
  expect_true(any(res$sp2_wrong$var1 != years_sp2))
})

