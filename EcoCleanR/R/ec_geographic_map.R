#' Map view of occurrence data points
#' @param data Data table
#' @param latitude default set to "decimalLatitude"
#' @param longitude default set to "decimalLongitude"
#'
#' @return A map view shows occurrence records.
#' @importFrom terra ext
#' @importFrom terra crop
#' @importFrom geodata world
#' @importFrom sf st_as_sf
#' @import ggplot2
#' @export
#'
#' @examples
#' data <- data.frame(
#'   scientificName = "Mexacanthina lugubris",
#'   decimalLongitude = c(-117, -117.8, -116.9),
#'   decimalLatitude = c(32.9, 33.5, 31.9),
#'   temperature_mean = c(12, 13, 14),
#'   temperature_min = c(9, 6, 10),
#'   temperature_max = c(14, 16, 18)
#' )
#' ec_geographic_map(data,
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude"
#' )
#'
ec_geographic_map <- function(data, latitude = "decimalLatitude", longitude = "decimalLongitude") {
  # Calculate latitude and longitude boundaries
  max_lat <- ceiling(max(data[[latitude]], na.rm = TRUE))
  min_lat <- floor(min(data[[latitude]], na.rm = TRUE))
  max_lon <- ceiling(max(data[[longitude]], na.rm = TRUE))
  min_lon <- floor(min(data[[longitude]], na.rm = TRUE))

  # Create geographic extent
  geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

  # Load and crop world map
  world_map <- geodata::world(resolution = 3, path = tempdir()) # Load world map
  cropped_map <- crop(x = world_map, y = geographic_extent) # Crop map

  # Convert cropped map to sf object
  my_map <- st_as_sf(cropped_map)

  # Plot the map using ggplot2
  ggplot(data = my_map) +
    geom_sf(fill = "grey95", color = "black") + # Plot the map
    geom_jitter(
      data = data,
      aes(x = .data[[longitude]], y = .data[[latitude]]),
      color = "#000066", size = 3.5, alpha = 0.5
    ) + # Add jittered points
    theme_minimal() + # Minimal theme
    labs(x = "Longitude", y = "Latitude", title = "Geographic Map") + # Labels and title
    theme(
      axis.text = element_text(size = 14), # Increase axis tick label size
      axis.title = element_text(size = 16), # Increase axis label size
      plot.title = element_text(size = 18, hjust = 0.5) # Center and enlarge the title
    )
}
