gstURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif"
gslURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif"
gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)

test_that("output type is correct", {
  temp <- generate_point_df(gst, gsl, 10, 6.4, 94)
  expect_s3_class(temp, "data.frame")
  expect_type(temp$longitude, "double")
  expect_type(temp$latitude, "double")
})

test_that("check input length", {
  expect_error(generate_point_df("gst", gsl, 10, 6.4, 94), "must be from class SpatRaster")
  expect_error(generate_point_df(gst, "gsl", 10, 6.4, 94), "must be from class SpatRaster")
  expect_error(generate_point_df(gst, gsl, "10", 6.4, 94), "must be numeric and finite")
  expect_error(generate_point_df(gst, gsl, NaN, 6.4, 94), "must be numeric and finite")
  expect_error(generate_point_df(gst, gsl, 10, "6.4", 94), "must be numeric and finite")
  expect_error(generate_point_df(gst, gsl, 10, NaN, 94), "must be numeric and finite")
  expect_error(generate_point_df(gst, gsl, 10, 6.4, "94"), "must be a integer and finite")
  expect_error(generate_point_df(gst, gsl, 10, 6.4, NaN), "must be a integer and finite")
})

test_that("check input type", {
  expect_error(generate_point_df(gst, gsl, c(1,2), 6.4, 94), "must be of length 1")
  expect_error(generate_point_df(gst, gsl, 10, c(1,2), 94), "must be of length 1")
  expect_error(generate_point_df(gst, gsl, 10, 6.4, c(1,2)), "must be of length 1")
})
