
osm_getbb <- function(place) osmdata::getbb(place)

osm_opq <- function(bbox) osmdata::opq(bbox = bbox)

osm_add_feature <- function(q, key, value = NULL) {
  osmdata::add_osm_feature(q, key = key, value = value)
}

osm_sf <- function(q, quiet = TRUE, ...) {
  osmdata::osmdata_sf(q, quiet = quiet, ...)
}
