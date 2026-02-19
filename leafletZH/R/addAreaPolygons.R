#' Add Area Polygons to a Map
#'
#' This function adds a polygon area to a given map using the specified latitude and longitude coordinates.
#'
#' @param map The map object to which the polygons will be added.
#' @param longitude A vector of longitudes.
#' @param latitude A vector of latitudes.
#' @param coordinate A string indicating the coordinate system of the input data. Options are "WGS-84", "GCJ-02", "BD-09".
#'
#' @return The updated map object with added polygons.
#' @export
#'
#' @examples
#' library(leaflet)
#' m <- leaflet() %>% addTilesAmap()
#' m <- addAreaPolygons(m,
#'   longitude = c(121.0, 122.1, 121.2, 122.15, 121.2),
#'   latitude =  c(31.1, 31.919, 31.917, 31.15, 31.12), coordinate = "WGS-84"
#' )
#' m
addAreaPolygons <- function(map, longitude, latitude, coordinate = "WGS-84") {
  # Check if latitude and longitude lengths match
  if (length(latitude) != length(longitude)) {
    stop("Latitude and longitude vectors must have the same length.")
  }

  # Validate the coordinate system
  if (!coordinate %in% c("WGS-84", "GCJ-02", "BD-09")) {
    stop("coordinate must be one of 'WGS-84', 'GCJ-02', and 'BD-09'")
  }

  # Create a data frame with the input coordinates
  data <- data.frame(longitude = longitude, latitude = latitude)

  # Convert coordinates to WGS-84 if necessary
  if (coordinate == "GCJ-02") {
    data <- purrr::map2_dfr(latitude, longitude, function(lat, lon) {
      coords <- convertCoordinates(lat, lon, from = "GCJ-02", to = "WGS-84")
      data.frame(longitude = coords[2], latitude = coords[1])
    })
  } else if (coordinate == "BD-09") {
    data <- purrr::map2_dfr(latitude, longitude, function(lat, lon) {
      coords <- convertCoordinates(lat, lon, from = "BD-09", to = "WGS-84")
      data.frame(longitude = coords[2], latitude = coords[1])
    })
  }

  # Remove duplicate points and create a convex hull to define the boundary
  data <- data[!duplicated(data), ]
  if (nrow(data) < 3) {
    stop("At least three points are required to form a polygon.")
  }

  # Compute the convex hull and get the order of points
  hull_index <- grDevices::chull(data$longitude, data$latitude)
  hull_data <- data[hull_index, ]

  # Add polygons to the map
  area <- round(areaCalculator(longitude, latitude, coordinate, chull = TRUE), 0)


  map <- leaflet::addPolygons(map,
    lng = hull_data$longitude,
    lat = hull_data$latitude,
    fillOpacity = 0.5,
    color = "blue",
    label = htmltools::HTML(paste("Area: ", scales::comma(area), " m<sup>2</sup>", sep = ""))
  )

  return(map)
}
