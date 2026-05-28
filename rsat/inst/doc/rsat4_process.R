## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----advanced_ima_data--------------------------------------------------------
#  library(rsat)
#  library(terra)
#  
#  data("ex.ndvi.navarre")
#  ex.ndvi.navarre <- rast(ex.ndvi.navarre)

## ----advanced_ima_show--------------------------------------------------------
#  library(tmap)
#  tm_shape(ex.ndvi.navarre) + tm_raster(title = "NDVI", style = "cont")

## ----advanced_ima-------------------------------------------------------------
#  library(rsat)
#  ndvi.fill <- rsat_smoothing_images(method = "IMA",
#                                     ex.ndvi.navarre,
#                                     nDays = 2,
#                                     nYears = 1,
#                                     fun = mean,
#                                     aFilter = c(0.01,0.99),
#                                     fact = 10,
#                                     only.na = TRUE)

## ----advanced_ima_result------------------------------------------------------
#  before <- ex.ndvi.navarre[[1:3]]
#  after <- ndvi.fill[[1:3]]
#  tm_shape(c(before,after)) + tm_raster(title = "NDVI", style = "cont")

