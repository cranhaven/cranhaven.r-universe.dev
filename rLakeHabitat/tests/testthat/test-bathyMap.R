#bathyMap test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- data.frame()

#input check
test_that("bathyMap input data check", {
  expect_error(bathyMap(wrong, contours = T, start = NULL, end = NULL, by = 5, breaks = NULL, units = "ft", labels = T, textSize = 1.5), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(bathyMap(dat, contours = T, start = NULL, end = NULL, by = 5, breaks = NULL, units = 2, labels = T, textSize = 1.5), info = "units must be a character")
  expect_error(bathyMap(dat, contours = T, start = NULL, end = NULL, by = 5, breaks = NULL, units = "ft", labels = T, textSize = wrong), info = "textSize must be numeric")
  expect_error(bathyMap(dat, contours = wrong, start = NULL, end = NULL, by = 5, breaks = NULL, units = "ft", labels = T, textSize = 1.5), info = "contours must be either 'T', 'F', TRUE, or FALSE")
  expect_error(bathyMap(dat, contours = T, start = NULL, end = NULL, by = NA, breaks = NULL, units = "ft", labels = T, textSize = 1.5), info = "'by' must be specified and numeric when including contours.")
  expect_error(bathyMap(dat, contours = T, start = NULL, end = NULL, by = 5, breaks = 3, 4, units = "ft", labels = T, textSize = 1.5), info = "breaks must be in vector format: c(1,2,3,...)")
  expect_error(bathyMap(dat, contours = T, start = NULL, end = NULL, by = 5, breaks = c(1,2,m), units = "ft", labels = T, textSize = 1.5), info = "not all elements of breaks are numeric")
  expect_error(bathyMap(dat, contours = F, start = NULL, end = NULL, by = 5, breaks = c(1,2,3), units = "ft", labels = T, textSize = 1.5), info = "contours must be T when including breaks")
})

#test output
test_that("bathyMap output check", {
  expect_s3_class(bathyMap(dat, units = "m", plotTitle = "Lake Monona"), class = "ggplot")
})
