#rarify test

#load test data
dat <- read.csv(system.file("extdata", "example_depths.csv", package = 'rLakeHabitat'))
hull <- terra::vect(system.file("extdata", "example_outline.shp", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- as.data.frame(matrix(NA, nrow=1,ncol=3))
colnames(wrong) <- c('x', 'y', 'z')
wrong[1,1:3] <- "1"

wrongvect <- list(wrong)

#input check
test_that("rarify input data check", {
  expect_error(rarify(hull, wrongvect, "x", "y", "z", res = 100), info = "df must be a dataframe")
  expect_error(rarify(hull, dat, 1, "y", "z", res = 100), info = "x must be a character giving the longitude column name")
  expect_error(rarify(hull, dat, "x", 1, "z", res = 100), info = "y must be a character giving the latitude column name")
  expect_error(rarify(hull, dat, "x", "y", 1, res = 100), info = "z must be a character giving the depth column name")
  expect_error(rarify(hull, dat, "long", "y", "z", res = 100), info = "The value of x does not appear to be a valid column name")
  expect_error(rarify(hull, dat, "x", "lat", "z", res = 100), info = "The value of y does not appear to be a valid column name")
  expect_error(rarify(hull, dat, "x", "y", "depth", res = 100), info = "The value of z does not appear to be a valid column name")
  expect_error(rarify(hull, wrong, "x", "y", "z", res = 100), info = "data in x column is not formatted as numeric")
  expect_error(rarify(hull, wrong, "x", "y", "z", res = 100), info = "data in y column is not formatted as numeric")
  expect_error(rarify(hull, wrong, "x", "y", "z", res = 100), info = "data in z column is not formatted as numeric")
  expect_error(rarify(wrongvect, dat, "x", "y", "z", res = 100), info = "outline is not a SpatVector or cannot be transformed")
  expect_error(rarify(hull, dat, "x", "y", "z", res = no), info = "res must be specified as a numeric value")
})

#test output
test_that("rarify output check", {
  expect_s3_class(rarify(hull, dat, "x", "y", "z", res = 100), class = "data.frame")
})
