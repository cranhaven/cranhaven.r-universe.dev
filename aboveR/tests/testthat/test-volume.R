# Tests for estimate_volume() and impoundment_curve()

dem_before_path  <- system.file("extdata/dem_before.tif", package = "aboveR")
dem_after_path   <- system.file("extdata/dem_after.tif", package = "aboveR")
dem_ref_path     <- system.file("extdata/dem_reference.tif", package = "aboveR")
boundary_path    <- system.file("extdata/boundary.gpkg", package = "aboveR")

test_that("estimate_volume() returns list with expected components", {
  skip_if(dem_after_path == "", message = "Sample data not installed")
  surface   <- terra::rast(dem_after_path)
  reference <- terra::rast(dem_before_path)
  boundary  <- sf::st_read(boundary_path, quiet = TRUE)

  vol <- estimate_volume(surface, reference, boundary)

  expect_type(vol, "list")
  expect_true(all(c("cut_volume_m3", "fill_volume_m3", "net_volume_m3",
                     "area_m2", "mean_depth_m", "method") %in% names(vol)))
  expect_equal(vol$method, "trapezoidal")
  expect_true(vol$cut_volume_m3 >= 0)
  expect_true(vol$fill_volume_m3 >= 0)
})

test_that("estimate_volume() accepts numeric reference (constant plane)", {
  skip_if(dem_after_path == "", message = "Sample data not installed")
  surface <- terra::rast(dem_after_path)

  vol <- estimate_volume(surface, 310)

  expect_type(vol, "list")
  expect_true(vol$area_m2 > 0)
})

test_that("estimate_volume() simpson method works", {
  skip_if(dem_after_path == "", message = "Sample data not installed")
  surface   <- terra::rast(dem_after_path)
  reference <- terra::rast(dem_before_path)

  vol <- estimate_volume(surface, reference, method = "simpson")
  expect_equal(vol$method, "simpson")
  expect_true(vol$area_m2 > 0)
})

test_that("impoundment_curve() returns data frame", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  dem      <- terra::rast(dem_before_path)
  boundary <- sf::st_read(boundary_path, quiet = TRUE)

  curve <- impoundment_curve(dem, boundary, n_steps = 5)

  expect_s3_class(curve, "data.frame")
  expect_true(all(c("elevation", "area_m2", "volume_m3") %in% names(curve)))
  expect_equal(nrow(curve), 5)
  # Volume should be monotonically non-decreasing
  expect_true(all(diff(curve$volume_m3) >= 0))
})
