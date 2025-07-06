#calcVolume test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- data.frame()

#input check
test_that("calcVolume input data check", {
  expect_error(calcVolume(wrong, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "ft", depthUnits = "ft", by = 1, stop = NULL), info = "DEMunits misspecified. Please choose either 'm', 'km', or 'ha'")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ha", by = 1, stop = NULL), info = "depthUnits misspecified. Please choose either 'm' or 'ft'")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 0, stop = NULL), info = "by can't be zero")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = m, stop = NULL), info = "by value must be numeric.")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 1, stop = m), info = "stop must be numeric")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 5, stop = 2), info = "stop cannot be less than 'by'")
  expect_error(calcVolume(dat, thermo_depth = m, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "thermo_depth must be numeric")
  expect_error(calcVolume(dat, thermo_depth = 0, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "thermo_depth can't be zero")
  expect_error(calcVolume(dat, thermo_depth = 30, thermo_high, thermo_low, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "thermo_depth cannot exceed maximum waterbody depth")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high = 1, thermo_low, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "Both thermo_high and thermo_low must be provided when thermo_depth is NULL")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high = m, thermo_low = m, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "thermo_low and thermo_high values must be numeric.")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high = 0, thermo_low = 0, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "thermo_low and thermo_high can't be zero.")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high = 6, thermo_low = 5, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "thermo_low must be greater than thermo_high.")
  expect_error(calcVolume(dat, thermo_depth = NULL, thermo_high = 2, thermo_low = 40, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "thermo_low and thermo_high cannot exceed maximum waterbody depth")
})

#test output
test_that("calcVolume output check", {
  expect_s3_class(calcVolume(dat, thermo_depth = 5, thermo_high, thermo_low, DEMunits = "m", depthUnits = "m", by = 10, stop = NULL), class = "data.frame")
})
