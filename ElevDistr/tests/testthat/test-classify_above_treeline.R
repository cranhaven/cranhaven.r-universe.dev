gstURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif"
gslURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif"
gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)
point <- data.frame("lon" = 8.65, "lat" = 46.87)

test_that("output type is correct", {
  temp <- classify_above_treeline(point, gst, gsl, 6.4, 94)
  expect_s3_class(temp, "data.frame")
  expect_type(temp$growingSeasonTemperature, "double")
  expect_type(temp$growingSeasonLength, "integer")
  expect_type(temp$aboveTreeline, "logical")
})

test_that("check input length", {
  expect_error(classify_above_treeline(as.data.frame(point[,1]), gst, gsl, 6.4, 94), "at least two columns")
  expect_error(classify_above_treeline(point, gst, gsl, c(1,2), 94), "must be of length 1")
  expect_error(classify_above_treeline(point, gst, gsl, 6.4, c(1,2)), "must be of length 1")
})

test_that("check input type", {
  expect_error(classify_above_treeline((point[,1]), gst, gsl, 6.4, 94), "must be a data frame")
  expect_error(classify_above_treeline(point, "gst", gsl, 6.4, 94), "must be from class SpatRaster")
  expect_error(classify_above_treeline(point, gst, "gsl", 6.4, 94), "must be from class SpatRaster")
  expect_error(classify_above_treeline(point, gst, gsl, "6.4", 94), "must be numeric and finite")
  expect_error(classify_above_treeline(point, gst, gsl, NaN, 94), "must be numeric and finite")
  expect_error(classify_above_treeline(point, gst, gsl, 6.4, "94"), "must be a integer and finite")
  expect_error(classify_above_treeline(point, gst, gsl, 6.4, NaN), "must be a integer and finite")

  point[,1] <- as.character(point[,1])
  expect_error(classify_above_treeline(point, gst, gsl, 6.4, 94), "must be numeric and finite")

  point <- data.frame("lon" = 8.65, "lat" = 46.87)
  point[1,1] <- NA
  expect_error(classify_above_treeline(point, gst, gsl, 6.4, 94), "must be numeric and finite")

  point[1,1] <- NaN
  expect_error(classify_above_treeline(point, gst, gsl, 6.4, 94), "must be numeric and finite")
})
