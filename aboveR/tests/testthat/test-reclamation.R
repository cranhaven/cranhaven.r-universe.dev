# Tests for reclamation_progress() and surface_roughness()

dem_before_path <- system.file("extdata/dem_before.tif", package = "aboveR")
dem_after_path  <- system.file("extdata/dem_after.tif", package = "aboveR")

test_that("reclamation_progress() returns list with expected components", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  current <- terra::rast(dem_after_path)
  target  <- terra::rast(dem_before_path)

  prog <- reclamation_progress(current, target, tolerance = 1)

  expect_type(prog, "list")
  expect_true(all(c("deviation", "on_grade_pct", "above_grade_pct",
                     "below_grade_pct", "mean_deviation", "rmse") %in% names(prog)))
  expect_s4_class(prog$deviation, "SpatRaster")
  # Percentages should sum to ~100
  total <- prog$on_grade_pct + prog$above_grade_pct + prog$below_grade_pct
  expect_equal(total, 100, tolerance = 0.01)
})

test_that("surface_roughness() returns raster", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_before_path)

  rough <- surface_roughness(dem, window = 3)

  expect_s4_class(rough, "SpatRaster")
  expect_equal(names(rough), "roughness")
})

test_that("surface_roughness() rejects even window", {
  skip_if(dem_before_path == "", message = "Sample data not installed")
  dem <- terra::rast(dem_before_path)
  expect_error(surface_roughness(dem, window = 4), "odd")
})
