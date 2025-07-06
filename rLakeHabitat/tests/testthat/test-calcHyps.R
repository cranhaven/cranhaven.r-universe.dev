#calcHyps test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- data.frame()

#input check
test_that("calcHyps input data checks", {
  expect_error(calcHyps(wrong, DEMunits = "m", depthUnits = "ft", by = 1, output = "values"), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(calcHyps(DEM, DEMunits = "ft", depthUnits = "ft", by = 1, output = "values"), info = "DEM units misspecified. Please choose either 'm', 'km', or 'ha'")
  expect_error(calcHyps(DEM, DEMunits = "m", depthUnits = "ft", by = 0, output = "values"), info = "by can't be zero")
  expect_error(calcHyps(DEM, DEMunits = "m", depthUnits = "ft", by = m, output = "values"), info = "by value must be numeric.")
  expect_error(calcHyps(DEM, DEMunits = "m", depthUnits = "ha", by = 1, output = "values"), info = "depthUnits not specified, must be 'ft' or 'm'")
  expect_error(calcHyps(DEM, DEMunits = "m", depthUnits = "ft", by = 1, output = NULL), info = "output not specified, must be 'values' or 'plot'")
})

#test output
test_that("calcHyps output check", {
  expect_s3_class(calcHyps(dat, DEMunits = "m", depthUnits = "m", by = 2, output = "values"), class = "data.frame")
  expect_s3_class(calcHyps(dat, DEMunits = "m", depthUnits = "m", by = 2, output = "plot"), class = "ggplot")
})
