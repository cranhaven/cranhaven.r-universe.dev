point <- as.list(c(8.728898, 46.9375))
temp <- generate_grid(8.728898, 46.93756, 10, 0.0025)

gstURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif"
gslURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif"
gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)

temp$df <- classify_above_treeline(temp$df, gst, gsl)
treeline <- sample_treeline(temp$df, temp$lonLength, temp$latLength, 0.0025)

test_that("check input length", {
  expect_error(plot_distr(as.list(point[1]), temp$df, treeline, 12), "must be of length 2")
  expect_error(plot_distr(point, temp$df[,1:2], treeline, 12), "needs to have at least three columns")
  expect_error(plot_distr(point, temp$df, treeline[,1:4], 12), "needs to have five columns")
  expect_error(plot_distr(point, temp$df, treeline, c(1,2)), "must be of length 1")
})

test_that("check input type", {
  expect_error(plot_distr("point", temp$df, treeline, 12), "must be a list")

  point[[1]] <- NA
  expect_error(plot_distr(point, temp$df, treeline, 12), "elements must be numeric and finite")
  point[[1]] <- NaN
  expect_error(plot_distr(point, temp$df, treeline, 12), "elements must be numeric and finite")
  point <- as.list(c(8.728898, 46.9375))
  point[[1]] <- as.character(point[[1]])
  expect_error(plot_distr(point, temp$df, treeline, 12), "elements must be numeric and finite")

  point <- as.list(c(8.728898, 46.9375))
  expect_error(plot_distr(point, as.list(temp$df), treeline, 12), "must be a data frame")

  temp$df[1,1] <- NA
  expect_error(plot_distr(point, temp$df, treeline, 12), "only finite numeric elements")
  temp$df[1,1] <- NaN
  expect_error(plot_distr(point, temp$df, treeline, 12), "only finite numeric elements")
  temp$df[1,1] <- "8.663898"
  expect_error(plot_distr(point, temp$df, treeline, 12), "only finite numeric elements")

  temp$df[,1] <- as.numeric(temp$df[,1])
  temp$df[1,5] <- "FALSE"
  expect_error(plot_distr(point, temp$df, treeline, 12), "only logical constants")

  temp$df[,5] <- as.logical(temp$df[,5])
  expect_error(plot_distr(point, temp$df, as.list(treeline), 12), "must be a data frame")

  treeline[1,2] <- NA
  expect_error(plot_distr(point, temp$df, treeline, 12), "must contain only finite numeric elements")
  treeline[1,2] <- NaN
  expect_error(plot_distr(point, temp$df, treeline, 12), "must contain only finite numeric elements")
  treeline[1,2] <- "46.91626"
  expect_error(plot_distr(point, temp$df, treeline, 12), "must contain only finite numeric elements")

  treeline[,2] <- as.numeric(treeline[,2])
  expect_error(plot_distr(point, temp$df, treeline, 12.2), "must be a finite integer")
  expect_error(plot_distr(point, temp$df, treeline, NaN), "must be a finite integer")

  expect_error(plot_distr(point, temp$df, treeline, 2), "must be from 3 to 21")
  expect_error(plot_distr(point, temp$df, treeline, 22), "must be from 3 to 21")
})
