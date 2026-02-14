## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(trafficCAR)
# 
# # Query a place name (uses osmdata::getbb() under the hood)
# roads_sf <- fetch_osm_roads("College Station, TX")
# 
# segments <- roads_to_segments(roads_sf)
# net <- build_network(segments)

## ----eval=FALSE---------------------------------------------------------------
# library(trafficCAR)
# 
# # xmin, ymin, xmax, ymax
# bbox <- c(-96.38, 30.59, -96.33, 30.63)
# roads_sf <- fetch_osm_roads(bbox)
# 
# segments <- roads_to_segments(roads_sf)
# net <- build_network(segments)

## ----eval=FALSE---------------------------------------------------------------
# roads_sf <- fetch_osm_roads(
#   "College Station, TX",
#   value = c("primary", "secondary"),
#   extra_tags = list(surface = "asphalt")
# )

