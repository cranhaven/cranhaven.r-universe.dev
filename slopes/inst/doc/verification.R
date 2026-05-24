## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slopes)
library(sf)

## ----eval=FALSE---------------------------------------------------------------
# download.file("https://ndownloader.figshare.com/files/14331185", "3DGRT_AXIS_EPSG25830_v2.zip")
# unzip("3DGRT_AXIS_EPSG25830_v2.zip")
# trace = sf::read_sf("3DGRT_AXIS_EPSG25830_v2.shp")
# plot(trace)
# nrow(trace)
# #> 11304
# summary(trace$X3DGRT_h)
# #>  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# #>   642.9   690.3   751.4   759.9   834.3   884.9

## ----eval=FALSE, echo=FALSE---------------------------------------------------
# # original trace dataset
# traces = sf::read_sf("vignettes/3DGRT_TRACES_EPSG25830_v2.shp")
# traces = sf::read_sf("3DGRT_TRACES_EPSG25830_v2.shp")
# nrow(traces)
# #> [1] 111113

## -----------------------------------------------------------------------------
res_gps = c(0.00, 4.58, 1136.36, 6.97)
res_final = c(0.00, 4.96, 40.70, 3.41)
res = data.frame(cbind(
  c("GPS", "Dual frequency GNSS receiver"),
  rbind(res_gps, res_final)
))
names(res) = c("Source", "min", " mean", " max", " stdev")
knitr::kable(res, row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# # mapview::mapview(trace) # check extent: it's above 6km in height
# # remotes::install_github("hypertidy/ceramic")
# loc = colMeans(sf::st_coordinates(sf::st_transform(trace, 4326)))
# e = ceramic::cc_elevation(loc = loc[1:2], buffer = 3000)
# trace_projected = sf::st_transform(trace, 3857)
# plot(e)
# plot(trace_projected$geometry, add = TRUE)

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# # aim: get max distance from centrepoint
# bb = sf::st_bbox(sf::st_transform(trace, 4326))
# geosphere::distHaversine(c(bb[1], bb[2]), c(bb[3], bb[2]))
# geosphere::distHaversine(c(bb[1], bb[2]), c(bb[1], bb[4]))
# # max of those 2 and divide by 2

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("https://user-images.githubusercontent.com/1825120/81125221-75c06780-8f2f-11ea-8cea-ad6322ef99e7.png")

## ----eval=FALSE---------------------------------------------------------------
# # source: https://www.robinlovelace.net/presentations/munster.html#31
# points2line_trajectory = function(p) {
#   c = st_coordinates(p)
#   i = seq(nrow(p) - 2)
#   l = purrr::map(i, ~ sf::st_linestring(c[.x:(.x + 1), ]))
#   lfc = sf::st_sfc(l)
#   a = seq(length(lfc)) + 1 # sequence to subset
#   p_data = cbind(sf::st_set_geometry(p[a, ], NULL))
#   sf::st_sf(p_data, geometry = lfc)
# }
# r = points2line_trajectory(trace_projected)
# # summary(st_length(r)) # mean distance is 1m! Doesn't make sense, need to create segments
# s = slope_raster(r, e = e)
# slope_summary = data.frame(min = min(s), mean = mean(s), max = max(s), stdev = sd(s))
# slope_summary = slope_summary * 100
# knitr::kable(slope_summary, digits = 1)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
# # failed tests
# raster::extract(e, trace_projected)
# raster::writeRaster(e, "e.tif")
# e_terra = terra::rast("e.tif")
# terra::crs(e_terra)
# v = terra::vect("vignettes/3DGRT_TRACES_EPSG25830_v2.shp")
# e_wgs = terra::project(e_terra, v)
# e_stars = stars::st_as_stars(e)
# e_wgs = sf::st_transform(e_stars, 4326)
# stars::write_stars(e_wgs, "e_wgs.tif")
# e2 = raster::raster("e_wgs.tif")
# raster::plot(e)
# plot(trace$geometry, add = TRUE)

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# # discarded way:
# remotes::install_github("jhollist/elevatr")
# sp_bbox = sp::bbox(sf::as_Spatial(sf::st_transform(trace, 4326)))
# e = elevatr::get_aws_terrain(locations = sp_bbox, prj = "+init:4326")

