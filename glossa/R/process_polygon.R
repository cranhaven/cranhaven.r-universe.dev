#' Enlarge/Buffer a Polygon
#'
#' This function enlarges a polygon by applying a buffer.
#'
#' @param polygon An sf object representing the polygon to be buffered.
#' @param buffer_distance Numeric. The buffer distance in decimal degrees (arc degrees).
#' @return An sf object representing the buffered polygon.
#'
#' @export
buffer_polygon <- function(polygon, buffer_distance) {
  if (!(inherits(polygon, "sf") | inherits(polygon, "sfc") | inherits(polygon, "sfc_MULTIPOLYGON") | inherits(polygon, "sfc_POLYGON"))) {
    stop("Input must be an 'sf' object representing a polygon.")
  }

  if (!is.numeric(buffer_distance) || length(buffer_distance) != 1) {
    stop("Buffer distance must be a single numeric value.")
  }

  buffered_polygon <- sf::st_buffer(polygon, buffer_distance)
  return(sf::st_geometry(buffered_polygon))
}


#' Invert a Polygon
#'
#' This function inverts a polygon by calculating the difference between the bounding box and the polygon.
#'
#' @param polygon An sf object representing the polygon to be inverted.
#' @param bbox Optional. An sf or bbox object representing the bounding box. If NULL, the bounding box of the input polygon is used.
#' @return An sf object representing the inverted polygon.
#'
#' @export
invert_polygon <- function(polygon, bbox = NULL) {
  if (!(inherits(polygon, "sf") | inherits(polygon, "sfc") | inherits(polygon, "sfc_MULTIPOLYGON") | inherits(polygon, "sfc_POLYGON"))) {
    stop("Input must be an 'sf' object representing a polygon.")
  }

  # Create bounding box polygon if bbox is not provided
  if (is.null(bbox)) {
    bbox <- sf::st_bbox(polygon)
    bbox_poly <- sf::st_as_sfc(bbox)
  } else {
    bbox_poly <- sf::st_as_sfc(sf::st_bbox(bbox))
    sf::st_crs(bbox_poly) <- sf::st_crs(polygon)
  }

  # If they are the same buffer a little bit the bounding box to create a frame.
  equal <- tryCatch({
    length(sf::st_equals(bbox_poly, polygon)[[1]]) > 0
  }, error = function(e) FALSE)
  if (equal){
    bbox_poly <- suppressWarnings(sf::st_buffer(bbox_poly, 0.1))
  }

  # When it's projected sf has issues computing the difference
  # An alternative is to change sf_use_s2() to FALSE
  #crs <- sf::st_crs(polygon)
  #sf::st_crs(polygon) <- NA
  #sf::st_crs(bbox_poly) <- NA
  sf::sf_use_s2(FALSE)

  # Invert the polygon
  inverted_polygon <- sf::st_difference(bbox_poly, polygon)
  #sf::st_crs(inverted_polygon) <- crs

  return(inverted_polygon)
}
