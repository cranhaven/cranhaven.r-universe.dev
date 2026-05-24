## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slopes)
library(bench)
library(raster)

## ----results='hide'-----------------------------------------------------------
e = dem_lisbon_raster
r = lisbon_road_network
et = terra::rast(e)
res = bench::mark(check = FALSE,
  slope_raster = slope_raster(r, e),
  slope_terra = slope_raster(r, et)
)

## -----------------------------------------------------------------------------
res

## -----------------------------------------------------------------------------
round(res$`itr/sec` * nrow(r))

## ----results='hide'-----------------------------------------------------------
e = dem_lisbon_raster
r = lisbon_road_network
res = bench::mark(check = FALSE,
  bilinear1 = slope_raster(r, e),
  bilinear2 = slope_raster(r, et),
  simple1 = slope_raster(r, e, method = "simple"),
  simple2 = slope_raster(r, et, method = "simple")
)

## -----------------------------------------------------------------------------
res

## -----------------------------------------------------------------------------
round(res$`itr/sec` * nrow(r))

