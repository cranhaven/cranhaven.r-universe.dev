gstURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gst_1981-2010_V.2.1.tif"
gslURL <- "https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_gsl_1981-2010_V.2.1.tif"
gst <- terra::rast(gstURL, vsi = TRUE)
gsl <- terra::rast(gslURL, vsi = TRUE)

gmted2010URL <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Global_tiles_GMTED/300darcsec/med/E000/30N000E_20101117_gmted_med300.tif"
gmted2010Part <- terra::rast(gmted2010URL, vsi = TRUE)

test_that("output is correct", {
  #Regular output
  expect_equal(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), -1491)
  expect_type(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "double")
  expect_length(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                     gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                     gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                     plotHist = FALSE, gstMin = 6.4, gslMin = 94), 1)

  #If vectors are given as a input
  lonV <- c(8.65, 8.65, 8.65)
  latV <- c(46.87, 46.87, 46.87)
  eleV <- c(504, 504, 504)
  expect_length(distance_to_treeline(lon = lonV, lat = latV, pointDf = pointsAboveTreeline, gstRaster = gst,
                                     gslRaster = gsl, elevationRaster = gmted2010Part, elevation = eleV, gridSize = 10,
                                     gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                     plotHist = FALSE, gstMin = 6.4, gslMin = 94), 3)

})

test_that("check input length", {
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = as.data.frame(pointsAboveTreeline[,1]), gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "needs to have at least two column")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = c(1,2),
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "be of length 1")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = c(1,2), plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "be of length 1")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = TRUE, plotZoom = c(1,2), treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "be of length 1")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = c(1,2),
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "be of length 1")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = c(1,2), gslMin = 94), "be of length 1")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = c(1,2)), "be of length 1")
})

test_that("check input type", {
  expect_error(distance_to_treeline(lon = "8.65", lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = NaN, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = "46.87", pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = NaN, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = as.list(pointsAboveTreeline), gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be a data frame")
  temp <- pointsAboveTreeline
  temp[1,1] <- NA
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = temp, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must contain only numeric and finite elements")

  temp[1,1] <- "NA"
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = temp, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must contain only numeric and finite elements")

  temp[1,1] <- NaN
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = temp, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must contain only numeric and finite elements")

  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = "gst",
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be from class SpatRaster")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = "gsl", elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be from class SpatRaster")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = "gmted2010Part", elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be from class SpatRaster")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = "504", gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = NaN, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = "10",
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = NaN,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = "0.0025", plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = NaN, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = "FALSE", plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be a boolean")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = TRUE, plotZoom = 4.5, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be a integer and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = TRUE, plotZoom = NaN, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be a integer and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = TRUE, plotZoom = 2, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be from 3 to 21")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = TRUE, plotZoom = 22, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be from 3 to 21")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10.1,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be a integer and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = NaN,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94), "must be a integer and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = "FALSE", gstMin = 6.4, gslMin = 94), "must be a boolean")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = "6.4", gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = NaN, gslMin = 94), "must be numeric and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = 94.1), "must be a integer and finite")
  expect_error(distance_to_treeline(lon = 8.65, lat = 46.87, pointDf = pointsAboveTreeline, gstRaster = gst,
                                    gslRaster = gsl, elevationRaster = gmted2010Part, elevation = 504, gridSize = 10,
                                    gridStepSize = 0.0025, plot = FALSE, plotZoom = NULL, treelineSamplingSize = 10,
                                    plotHist = FALSE, gstMin = 6.4, gslMin = NaN), "must be a integer and finite")
})
