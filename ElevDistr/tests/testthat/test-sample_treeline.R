temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)

gstURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif"
gslURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif"
gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)

temp$df <- classify_above_treeline(temp$df, gst, gsl)

test_that("output type is correct", {
  treeline <- sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025)
  expect_s3_class(treeline, "data.frame")
  expect_type(treeline$lat1, "double")
  expect_type(treeline$lon1, "double")
  expect_type(treeline$lat2, "double")
  expect_type(treeline$lon2, "double")
})

test_that("check input length", {
  expect_error(sample_treeline(as.data.frame(temp$df[,1]), temp$lonLength, temp$latLength, 0.0025), "at least three columns")
  expect_error(sample_treeline(temp$df, c(1,2), temp$latLength, 0.0025), "must be of length 1")
  expect_error(sample_treeline(temp$df, temp$lonLength, c(1,2), 0.0025), "must be of length 1")
  expect_error(sample_treeline(temp$df, temp$lonLength, temp$latLength, c(1,2)), "must be of length 1")
})

test_that("check input type", {
  expect_error(sample_treeline(as.list(temp$df), temp$lonLength, temp$latLength, 0.0025), "must be a data frame")

  temp$df[,1] <- as.character(temp$df[,1])
  expect_error(sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025), "must contain only finite intagers in the 1st and 2nd column")

  temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)
  temp$df <- classify_above_treeline(temp$df, gst, gsl)
  temp$df[1,1] <- NA
  expect_error(sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025), "must contain only finite intagers in the 1st and 2nd column")

  temp$df[1,1] <- NaN
  expect_error(sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025), "must contain only finite intagers in the 1st and 2nd column")

  temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)
  temp$df <- classify_above_treeline(temp$df, gst, gsl)
  temp$df[,5] <- as.character(temp$df[,5])
  expect_error(sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025), "must contain only logical constants")

  temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)
  temp$df <- classify_above_treeline(temp$df, gst, gsl)
  expect_error(sample_treeline(temp$df, "temp$lonLength", temp$latLength, 0.0025), "must be numeric and finite")
  expect_error(sample_treeline(temp$df, NaN, temp$latLength, 0.0025), "must be numeric and finite")
  expect_error(sample_treeline(temp$df, temp$lonLength, "temp$latLength", 0.0025), "must be numeric and finite")
  expect_error(sample_treeline(temp$df, temp$lonLength, NaN, 0.0025), "must be numeric and finite")
  expect_error(sample_treeline(temp$df, temp$lonLength, temp$latLength, "0.0025"), "must be numeric and finite")
  expect_error(sample_treeline(temp$df, temp$lonLength, temp$latLength, NaN), "must be numeric and finite")
})
