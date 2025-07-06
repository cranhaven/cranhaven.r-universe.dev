#interpBathy test

#load test data
dat <- read.csv(system.file("extdata", "example_depths.csv", package = 'rLakeHabitat'))
hull <- terra::vect(system.file("extdata", "example_outline.shp", package = 'rLakeHabitat'))

#create incorrect data type
wrong <- as.data.frame(matrix(NA, nrow=1,ncol=3))
colnames(wrong) <- c('x', 'y', 'z')
wrong[1,1:3] <- "1"

wrongvect <- list(wrong)

#input check
test_that("interpBathy input data check", {
  expect_error(interpBathy(hull, wrongvect, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "df must be a dataframe")
  expect_error(interpBathy(hull, dat, "long", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "x must be a character giving the longitude column name")
  expect_error(interpBathy(hull, dat, "x", "lat", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "y must be a character giving the latitude column name")
  expect_error(interpBathy(hull, dat, "x", "y", "depth", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "z must be a character giving the depth column name")
  expect_error(interpBathy(hull, dat, "long", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "The value of x does not appear to be a valid column name")
  expect_error(interpBathy(hull, dat, "x", "lat", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "The value of y does not appear to be a valid column name")
  expect_error(interpBathy(hull, dat, "x", "y", "depth", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "The value of z does not appear to be a valid column name")
  expect_error(interpBathy(hull, wrong, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "data in x column is not formatted as numeric")
  expect_error(interpBathy(hull, wrong, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "data in y column is not formatted as numeric")
  expect_error(interpBathy(hull, wrong, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "data in z column is not formatted as numeric")
  expect_error(interpBathy(wrongvect, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "outline is not a SpatVector or cannot be transformed")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = no, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "zeros must be either 'T', 'F', TRUE, or FALSE")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = no, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "separation value must be specified")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = T, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "separation must be null if zeros = T")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = 10, res = NULL, method = "IDW", nmax = 4, idp = 2), info = "crsUnits must be a character (e.g., 'dd', 'm')")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "two", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "crsUnits must be either 'm' or 'dd'")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = no, method = "IDW", nmax = 4, idp = 2), info = "res must be numeric")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NA, method = "IDW", nmax = 4, idp = 2), info = "both crsUnits and res must be specified")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "no", nmax = 4, idp = 2), info = "method misspecified. Please choose either 'IDW' or 'OK'")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = no, idp = 2), info = "nmax must be numeric")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = no), info = "idp must be numeric")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = NA, idp = 2), info = "nmax must be specified")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = NA), info = "idp must be specified")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 20, idp = 2), info = "nmax cannot exceed number of observations in df")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "OK", nmax = 4, model = "no", psill = NULL, range = NULL, nugget = 0, kappa = NULL), info = "model must be character string of either 'Sph', 'Exp', 'Gau', or 'Mat'")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "OK", nmax = NA, model = "Sph", psill = NULL, range = NULL, nugget = 0, kappa = NULL), info = "nmax must be specified")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "OK", nmax = 4, model = "Sph", psill = NULL, range = NULL, nugget = no, kappa = NULL), info = "nugget must be numeric")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "OK", nmax = 4, model = "Sph", psill = NULL, range = no, nugget = 0, kappa = NULL), info = "range must be numeric")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "OK", nmax = 4, model = "Sph", psill = no, range = NULL, nugget = 0, kappa = NULL), info = "psill must be numeric")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "OK", nmax = 4, model = "Sph", psill = NULL, range = NULL, nugget = 0, kappa = no), info = "kappa must be numeric")
  expect_error(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = "dd", res = NULL, method = "IDW", nmax = 4, idp = 2), info = "CRS of 'outline' is unable to be defined.")
})

#test output
test_that("interpBathy output check", {
  expect_s4_class(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = 'dd', res = 50, method = "IDW", nmax = 4, idp = 2), class = "SpatRaster")
  expect_s4_class(interpBathy(hull, dat, "x", "y", "z", zeros = F, separation = 10, crsUnits = 'dd', res = 100, method = "OK", nmax = 4, model = "Exp", psill = 12, range = 0.01, kappa = .5)[[1]], class = "SpatRaster")
})
