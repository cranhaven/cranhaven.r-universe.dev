#trueSA test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#wrong data
wrong <- data.frame()

#input check
test_that("trueSA input data check", {
  expect_error(trueSA(wrong, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = F, photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(trueSA(dat, DEMunits = "wrong", CRSunits = "radians", neighbors = 4, littoral = F, photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "DEMunits misspecified. Please choose 'm', 'km', or 'ha'")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "wrong", neighbors = 4, littoral = F, photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "CRSunits misspecified. Please choose 'radians' or 'degrees'")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = m, littoral = F, photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "neighbors must be either 4 or 8")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 5, littoral = F, photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "neighbors must be either 4 or 8")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = "no", photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "littoral must be either 'T' 'F', 'TRUE', or 'FALSE'")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = NULL, depthUnits = "no", by = 1, stop = NULL), info = "depthUnits misspecified. Please choose 'm' or 'ft'")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = NULL, depthUnits = "m", by = 0, stop = NULL), info = "by can't be zero")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = NULL, depthUnits = "m", by = no, stop = NULL), info = "by value must be numeric.")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = "no"), info = "stop must be numeric")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = NULL, depthUnits = "m", by = 3, stop = 2), info = "stop cannot be less than by")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "Either secchi or photic must be defined when including littoral area.")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = "no", secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "photic must be numeric")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = 0, secchi = NULL, depthUnits = "m", by = 1, stop = NULL), info = "photic cannot be zero")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = "no", depthUnits = "m", by = 1, stop = NULL), info = "secchi must be numeric")
  expect_error(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = 0, depthUnits = "m", by = 1, stop = NULL), info = "secchi cannot be zero")
})

#test output
test_that("trueSA output check", {
  expect_s3_class(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = T, photic = NULL, secchi = 1, depthUnits = "m", by = 1, stop = NULL), class = "data.frame")
  expect_type(trueSA(dat, DEMunits = "m", CRSunits = "radians", neighbors = 4, littoral = F), type = "double")
})
