#genStack test

#load test tif
dat <- terra::rast(system.file("extdata", "example_raster.tif", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- data.frame()

#input check
test_that("genStack input data check", {
  expect_error(genStack(wrong, by = 1, stop = NULL, save = T, file_name = NULL, file_type = NULL), info = "DEM must be a SpatRaster object. Convert using 'rast' function in package 'terra'.")
  expect_error(genStack(dat, by = 0, stop = NULL, save = T, file_name = NULL, file_type = NULL), info = "by can't be zero")
  expect_error(genStack(dat, by = m, stop = NULL, save = T, file_name = NULL, file_type = NULL), info = "by value must be numeric.")
  expect_error(genStack(dat, by = 1, stop = NULL, save = T, file_name = NULL, file_type = NULL), info = "save must be either 'T', 'F', TRUE, or FALSE")
  expect_error(genStack(dat, by = 1, stop = NULL, save = T, file_name = NULL, file_type = NULL), info = "file_name cannot be NULL when save = T")
  expect_error(genStack(dat, by = 1, stop = NULL, save = T, file_name = NULL, file_type = NULL), info = "file_type cannot be NULL when save = T")
  expect_error(genStack(dat, by = 1, stop = NULL, save = T, file_name = 1, file_type = NULL), info = "file_name must be a character")
  expect_error(genStack(dat, by = 1, stop = NULL, save = T, file_name = NULL, file_type = 2), info = "file_type must be a character")
  expect_error(genStack(dat, by = 1, stop = NULL, save = T, file_name = NULL, file_type = "bad"), info = "file_type not accepted, use 'gdal(drivers = T)' to see accepted file types")
  expect_error(genStack(dat, by = 1, stop = m, save = T, file_name = NULL, file_type = NULL), info = "stop must be numeric")
  expect_error(genStack(dat, by = 5, stop = 2, save = T, file_name = NULL, file_type = NULL), info = "stop cannot be less than 'by'")
})


#test output
test_that("genStack output check", {
  expect_s4_class(genStack(dat, by = 10, stop = NULL, save = F, file_name = NULL, file_type = NULL), class = "SpatRaster")
})
