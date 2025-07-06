#animBathy input data check

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- data.frame()

test_that("animBathy input data check", {
  expect_error(animBathy(DEM = wrong, units = "ha", littoral = T, secchi = 28, photic = NULL, stop = 2, by = 1), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(animBathy(DEM = dat, units = "ha", littoral = T, secchi = 28, photic = NULL, stop = 2, by = 1), info = "Units misspecified. Please choose 'm' or 'ft'")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = 28, photic = NULL, stop = 2, by = NA), info = "by must be specified")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = 28, photic = NULL, stop = 2, by = "wrong"), info = "by value must be numeric.")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = 28, photic = NULL, stop = "wrong", by = 1), info = "stop must be numeric")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = 28, photic = NULL, stop = 1, by = 2), info = "stop cannot be less than 'by'")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = "wrong", secchi = 28, photic = NULL, stop = 2, by = 1), info = "littoral must be logical: 'T', 'TRUE', 'F', or 'FALSE'")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = NULL, photic = NULL, stop = 2, by = 1), info = "Either secchi or photic must be defined when including littoral area.")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = "wrong", photic = NULL, stop = 2, by = 1), info = "secchi must be numeric")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = 0, photic = NULL, stop = 2, by = 1), info = "secchi cannot be zero")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = NULL, photic = "wrong", stop = 2, by = 1), info = "photic must be numeric")
  expect_error(animBathy(DEM = dat, units = "ft", littoral = T, secchi = NULL, photic = 0, stop = 2, by = 1), info = "photic cannot be zero")
})

#test output
test_that("animBathy output check", {
  expect_s3_class(animBathy(dat, units = "m", littoral = T, secchi = 1, stop = 3), class = "gganim")
})
