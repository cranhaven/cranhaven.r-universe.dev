## ----setup, include = FALSE----------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 8, 
  fig.height = 5, 
  purl = NOT_CRAN,
  eval = NOT_CRAN
)

## ----load-roi-shapefile--------------------------------------------------
library(sf)
library(raster)
library(viridis)
library(leri)
roi <- st_read(system.file("shape/nc.shp", package="sf"))

## ----print-roi-shapefile-------------------------------------------------
roi

## ------------------------------------------------------------------------
roi <- st_union(roi)
roi

## ----get-data------------------------------------------------------------
leri_raster <- get_leri(date = "2018-08-13", product = "8 day ac")

## ----inspect-leri--------------------------------------------------------
leri_raster

## ----plot-leri-conus-----------------------------------------------------
plot(leri_raster, col = cividis(255))

## ----reproject-to-same-crs-----------------------------------------------
roi_reprojected <- st_transform(roi, crs = projection(leri_raster))

## ----plot-leri-with-shp--------------------------------------------------
plot(leri_raster, col = cividis(255))
plot(roi_reprojected, add = TRUE)

## ----mask-leri-----------------------------------------------------------
roi_sp <- as(roi_reprojected, 'Spatial')
cropped_leri <- crop(leri_raster, roi_sp)
masked_leri <- mask(cropped_leri, roi_sp)

## ------------------------------------------------------------------------
plot(masked_leri, col = cividis(255))
plot(roi_sp, add = TRUE)

## ----write-tif, eval = FALSE---------------------------------------------
#  writeRaster(masked_leri, 'leri-over-roi.tif')

