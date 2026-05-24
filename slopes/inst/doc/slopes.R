## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("ropensci/slopes")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("ropensci/slopes", dependencies = "Suggests")

## ----eval=FALSE---------------------------------------------------------------
# usethis::edit_r_environ()
# # Then add the following line to the file that opens:
# # MAPBOX_API_KEY=xxxxx # replace XXX with your api key

## -----------------------------------------------------------------------------
library(slopes)
library(sf)
library(raster)

# Load example data
data(lisbon_route)
data(dem_lisbon_raster)

## -----------------------------------------------------------------------------
sf_linestring_xyz_local = elevation_add(lisbon_route, dem = dem_lisbon_raster)
head(sf::st_coordinates(sf_linestring_xyz_local))

## ----eval=FALSE---------------------------------------------------------------
# # Requires a MapBox API key and the ceramic package
# # sf_linestring_xyz_mapbox = elevation_add(lisbon_route)
# # head(sf::st_coordinates(sf_linestring_xyz_mapbox))

## -----------------------------------------------------------------------------
slope = slope_xyz(sf_linestring_xyz_local)
slope

## -----------------------------------------------------------------------------
plot_slope(sf_linestring_xyz_local)

## -----------------------------------------------------------------------------
lisbon_route_segments = sf::st_segmentize(lisbon_route, dfMaxLength = 100) # Arbitrary length
lisbon_route_segments = sf::st_cast(lisbon_route_segments, "LINESTRING")
# Add elevation to segments
lisbon_route_segments_xyz = elevation_add(lisbon_route_segments, dem = dem_lisbon_raster)

## -----------------------------------------------------------------------------
lisbon_route_segments_xyz$slope = slope_xyz(lisbon_route_segments_xyz)
summary(lisbon_route_segments_xyz$slope)

## ----eval=FALSE---------------------------------------------------------------
# # Requires tmap package
# # library(tmap)
# # qtm(lisbon_route_segments_xyz, lines.col = "slope", lines.lwd = 3)

## -----------------------------------------------------------------------------
plot(st_geometry(lisbon_route_segments_xyz), col = heat.colors(length(lisbon_route_segments_xyz$slope))[rank(lisbon_route_segments_xyz$slope)], lwd = 3)

