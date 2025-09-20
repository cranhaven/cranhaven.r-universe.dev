temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)

gstURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif"
gslURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif"
gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)

gmted2010URL <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Global_tiles_GMTED/300darcsec/med/E000/30N000E_20101117_gmted_med300.tif"
gmted2010Part <- terra::rast(gmted2010URL, vsi = TRUE)

temp$df <- classify_above_treeline(temp$df, gst, gsl)
treeline <- sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025)

test_that("output is correct", {
  expect_equal(calculate_distance(treeline, gmted2010Part, 504, 10, FALSE), -1491)
  expect_type(calculate_distance(treeline, gmted2010Part, 504, 10, FALSE), "double")
  expect_length(calculate_distance(treeline, gmted2010Part, 504, 10, FALSE), 1)
})

test_that("check input length", {
  expect_error(calculate_distance(treeline[,-1], gmted2010Part, 504, 10, FALSE), "needs to have five columns")
  expect_error(calculate_distance(treeline, gmted2010Part, c(1,2), 10, FALSE), "must be of length 1")
  expect_error(calculate_distance(treeline, gmted2010Part, 504, c(1,2), FALSE), "must be of length 1")
})

test_that("check input type", {
  expect_error(calculate_distance(as.list(treeline), gmted2010Part, 504, 10, FALSE), "must be a data frame")

  treeline[1,1] <- NA
  expect_error(calculate_distance(treeline, gmted2010Part, 504, 10, FALSE), "can not contain NA")

  treeline <- sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025)
  treeline[,2] <- as.character(treeline[,2])
  expect_error(calculate_distance(treeline, gmted2010Part, 504, 10, FALSE), "must contain only numeric and finite elements")
  treeline[1,2] <- NaN
  expect_error(calculate_distance(treeline, gmted2010Part, 504, 10, FALSE), "must contain only numeric and finite elements")

  treeline <- sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025)
  expect_error(calculate_distance(treeline, "gmted2010Part", 504, 10, FALSE), "must be from class SpatRaster")
  expect_error(calculate_distance(treeline, gmted2010Part, "504", 10, FALSE), "must be numeric and finite")
  expect_error(calculate_distance(treeline, gmted2010Part, NaN, 10, FALSE), "must be numeric and finite")
  expect_error(calculate_distance(treeline, gmted2010Part, 504, "10", FALSE), "treelineSampling must be a integer and finite")
  expect_error(calculate_distance(treeline, gmted2010Part, 504, NaN, FALSE), "treelineSampling must be a integer and finite")
  expect_error(calculate_distance(treeline, gmted2010Part, 504, 0, FALSE), "can not be zero")
  expect_error(calculate_distance(treeline, gmted2010Part, 504, 10, "FALSE"), "must be a boolean")
})
