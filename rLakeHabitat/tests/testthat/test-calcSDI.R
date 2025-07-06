#calcSDI test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- data.frame()

#input check
test_that("calcSDI input data checks", {
  expect_error(calcSDI(wrong, units = "m", by = 1, stop = NULL), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(calcSDI(dat, units = "ft", by = 1, stop = NULL), info = "Units misspecified. Please choose 'm', 'km', or 'ha'")
  expect_error(calcSDI(dat, units = "m", by = 0, stop = NULL), info = "by can't be zero")
  expect_error(calcSDI(dat, units = "m", by = m, stop = NULL), info = "by value must be numeric")
  expect_error(calcSDI(dat, units = "m", by = 1, stop = m), info = "stop must be numeric")
  expect_error(calcSDI(dat, units = "m", by = 5, stop = 2), info = "stop cannot be less than 'by'")
})

#test output
test_that("calcSDI output check", {
  expect_s3_class(calcSDI(dat, units = "m", by = 10, stop = NULL), class = "data.frame")
})
