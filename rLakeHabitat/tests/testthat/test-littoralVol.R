#littoralVol test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#wrong data
wrong <- data.frame()

#input check
test_that("littoralVol input data check", {
  expect_error(littoralVol(wrong, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(littoralVol(dat, photic, secchi = NULL, DEMunits = "ft", depthUnits = "ft", by = 1), info = "DEM units misspecified. Please choose 'm', 'km', or 'ha'")
  expect_error(littoralVol(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ha", by = 1), info = "Depth units misspecified. Please choose either 'm' or 'ft'")
  expect_error(littoralVol(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 0), info = "by can't be zero")
  expect_error(littoralVol(dat, photic, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = m), info = "by value must be numeric.")
  expect_error(littoralVol(dat, photic, secchi = m, DEMunits = "m", depthUnits = "ft", by = 1), info = "secchi must be numeric")
  expect_error(littoralVol(dat, photic, secchi = 0, DEMunits = "m", depthUnits = "ft", by = 1), info = "secchi cannot be zero")
  expect_error(littoralVol(dat, photic, secchi = 50, DEMunits = "m", depthUnits = "ft", by = 1), info = "secchi cannot exceed maximum waterbody depth")
  expect_error(littoralVol(dat, photic = m, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1), info = "photic must be numeric")
  expect_error(littoralVol(dat, photic = 0, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1), info = "photic cannot be zero")
  expect_error(littoralVol(dat, photic = 50, secchi = NULL, DEMunits = "m", depthUnits = "ft", by = 1), info = "photic cannot exceed maximum waterbody depth")
})

#test output
test_that("littoralVol output check", {
  expect_s3_class(littoralVol(dat, photic, secchi = 2, DEMunits = "m", depthUnits = "m", by = 10), class = "data.frame")
})
