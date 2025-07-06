#calcLittoral test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- data.frame()

#input check
test_that("calcLittoral input data check", {
  expect_error(calcLittoral(wrong, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(calcLittoral(dat, photic, secchi = NULL, DEMunits = "ft", depthUnits = "ft", by = 1, stop = NULL), info = "DEMunits misspecified. Please choose 'm', 'km', or 'ha'")
  expect_error(calcLittoral(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ha", by = 1, stop = NULL), info = "depthUnits misspecified. Please choose 'm' or 'ft'")
  expect_error(calcLittoral(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 0, stop = NULL), info = "by can't be zero")
  expect_error(calcLittoral(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = m, stop = NULL), info = "by value must be numeric.")
  expect_error(calcLittoral(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1, stop = m), info = "stop must be numeric")
  expect_error(calcLittoral(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 5, stop = 2), info = "stop cannot be less than 'by'")
  expect_error(calcLittoral(dat, photic, secchi = m, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "secchi must be numeric")
  expect_error(calcLittoral(dat, photic, secchi = 0, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "secchi cannot be zero")
  expect_error(calcLittoral(dat, photic, secchi = 50, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "secchi cannot exceed maximum waterbody depth")
  expect_error(calcLittoral(dat, photic = m, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "photic must be numeric")
  expect_error(calcLittoral(dat, photic = 0, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1, stop = NULL), info = "photic cannot be zero")
})

#test output
test_that("calcLittoral output check", {
  expect_s3_class(calcLittoral(dat, secchi = 2, DEMunits = "m", depthUnits = "m", by = 10, stop = NULL), class = "data.frame")
})
