#' Calculate the area of a polygon defined by latitude and longitude points
#'
#' This function takes in latitude and longitude vectors and calculates the area of the polygon
#' defined by those points. It can handle different coordinate systems such as WGS-84, GCJ-02, and BD-09.
#'
#' @param longitude A numeric vector of longitude points.
#' @param latitude A numeric vector of latitude points.
#' @param coordinate A string indicating the coordinate system of the input points, can be "WGS-84",
#'        "GCJ-02", or "BD-09". Default is "WGS-84".
#' @param chull A logical value indicating whether to use the convex hull of the points. Default is TRUE.
#'
#' @return A numeric value representing the area of the polygon in square meters.
#' @export
#'
#' @examples
#' area <- areaCalculator(
#'   longitude = c(121.0, 122.1, 121.2, 122.15, 121.2),
#'   latitude =  c(31.1, 31.919, 31.917, 31.15, 31.12), coordinate = "WGS-84"
#' )
areaCalculator <- function(longitude,
                           latitude,
                           coordinate = "WGS-84",
                           chull = TRUE) {
  # Check if latitude and longitude lengths match
  if (length(latitude) != length(longitude)) {
    stop("Latitude and longitude vectors must have the same length.")
  }
  if (!coordinate %in% c("WGS-84", "GCJ-02", "BD-09")) {
    stop("coordinate must be one of WGS-84, GCJ-02, and BD-09")
  }

  # Create a data frame with the input coordinates
  data <- data.frame(longitude = longitude, latitude = latitude)

  # Convert coordinates to WGS-84 if necessary
  if (coordinate == "GCJ-02") {
    data$latitude <- purrr::map2(
      .x = latitude,
      .y = longitude,
      \(x, y)convertCoordinates(
        latitude = x,
        longitude = y,
        from = "GCJ-02",
        to = "WGS-84"
      )[1]
    )
    data$longitude <- purrr::map2(
      .x = latitude,
      .y = longitude,
      \(x, y)convertCoordinates(
        latitude = x,
        longitude = y,
        from = "GCJ-02",
        to = "WGS-84"
      )[2]
    )
  } else if (coordinate == "BD-09") {
    data$latitude <- purrr::map2(
      .x = latitude,
      .y = longitude,
      \(x, y)convertCoordinates(
        latitude = x,
        longitude = y,
        from = "BD-09",
        to = "WGS-84"
      )[1]
    )
    data$longitude <- purrr::map2(
      .x = latitude,
      .y = longitude,
      \(x, y)convertCoordinates(
        latitude = x,
        longitude = y,
        from = "BD-09",
        to = "WGS-84"
      )[2]
    )
  }


  # Filter the data to include only the convex hull points if chull is TRUE
  if (chull) {
    data <- data[chull(data$longitude, data$latitude), ]
  }


  # Calculate the area of the polygon
  area <- geosphere::areaPolygon(data)

  return(area)
}
