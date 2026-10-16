#' Fetch road geometries from OpenStreetMap
#'
#' Convenience wrapper around `osmdata` to download road geometries and
#' return an `sf` object that can be passed into `roads_to_segments()`.
#'
#' @param place A character place name (passed to `osmdata::getbb()`) or a
#'   bounding box object accepted by `osmdata::opq()`.
#' @param key OSM feature key to query. Default is `"highway"`.
#' @param value Optional character vector of OSM feature values.
#'   For example, `c("primary", "secondary")`.
#' @param extra_tags Optional named list of additional tags passed to
#'   `osmdata::add_osm_feature()`.
#' @param layer Which OSM layer to return. Defaults to `"osm_lines"`.
#' @param quiet Logical; suppress osmdata messages. Default TRUE.
#' @param ... Additional arguments passed to `osmdata::osmdata_sf()`.
#'
#' @return An `sf` object with road geometries.
#' @export
fetch_osm_roads <- function(place,
                            key = "highway",
                            value = NULL,
                            extra_tags = NULL,
                            layer = c("osm_lines", "osm_multilines", "osm_polygons", "osm_multipolygons"),
                            quiet = TRUE,
                            ...) {
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    stop("Package 'osmdata' is required to fetch OpenStreetMap roads.")
  }

  if (is.character(place)) {
    if (length(place) != 1L) {
      stop("`place` must be a single place name or a bounding box object.")
    }
    bbox <- osm_getbb(place)
    if (is.null(bbox)) {
      stop("Unable to resolve `place` to a bounding box: ", place)
    }
  } else {
    bbox <- place
  }

  layer <- match.arg(layer)

  q <- osm_opq(bbox = bbox)
  q <- osm_add_feature(q, key = key, value = value)

  if (!is.null(extra_tags)) {
    if (!is.list(extra_tags) || is.null(names(extra_tags))) {
      stop("`extra_tags` must be a named list.")
    }
    for (tag_name in names(extra_tags)) {
      q <- osm_add_feature(q, key = tag_name, value = extra_tags[[tag_name]])
    }
  }

  osm <- osm_sf(q, quiet = quiet, ...)
  roads <- osm[[layer]]


  if (is.null(roads) || nrow(roads) == 0L) {
    stop("No road geometries returned for the supplied query.")
  }

  roads
}
