# Tests for terrain_profile() and boundary_terrain_profile()

dem_path      <- system.file("extdata/dem_before.tif", package = "aboveR")
line_path     <- system.file("extdata/profile_line.gpkg", package = "aboveR")
boundary_path <- system.file("extdata/boundary.gpkg", package = "aboveR")

test_that("terrain_profile() returns data frame with expected columns", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem  <- terra::rast(dem_path)
  line <- sf::st_read(line_path, quiet = TRUE)

  prof <- terrain_profile(dem, line)

  expect_s3_class(prof, "data.frame")
  expect_true(all(c("distance", "elevation", "x", "y") %in% names(prof)))
  expect_true(nrow(prof) > 2)
  # Distance should start at 0
  expect_equal(prof$distance[1], 0)
})

test_that("terrain_profile() accepts file path for line", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_path)

  prof <- terrain_profile(dem, line_path)
  expect_s3_class(prof, "data.frame")
  expect_true(nrow(prof) > 2)
})

test_that("boundary_terrain_profile() profiles polygon boundary", {
  skip_if(dem_path == "", message = "Sample data not installed")
  dem      <- terra::rast(dem_path)
  boundary <- sf::st_read(boundary_path, quiet = TRUE)

  bprof <- boundary_terrain_profile(dem, boundary)

  expect_s3_class(bprof, "data.frame")
  expect_true(all(c("distance", "elevation") %in% names(bprof)))
  expect_true(nrow(bprof) > 4)
})
