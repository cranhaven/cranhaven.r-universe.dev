#' Map view to visualize data points with outlier probability 0 to 1 on a map view

#' @param data Data table which has information of coordinates (decimalLongitude and decimalLatitude) and a column which has flags 0 to 1
#' @param flag_column column name which has flag, e.g. flag_outlier
#' @param latitude default set on "decimalLatitude", change if the name of column is different.
#' @param longitude default set on "decimalLongitude", change if the name of column is different.
#' @return A geographic map which shows occurrence data points with the color gradient to show flagged records in warm color.
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
#'   temperature_max = c(14, 16, 18),
#'   flag_outlier = c(0, 0.5, 1)
#' )
#' ec_geographic_map_w_flag(data,
#'   flag_column = "flag_outlier",
#'   latitude = "decimalLatitude",
#'   longitude = "decimalLongitude"
#' )
#'
ec_geographic_map_w_flag <- function(data, flag_column, latitude = "decimalLatitude", longitude = "decimalLongitude") {
  # Ensure the flag column is a factor
  data[[flag_column]] <- as.numeric(data[[flag_column]])

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
      aes(
        x = .data[[longitude]],
        y = .data[[latitude]],
        fill = .data[[flag_column]]
      ), # Fill with flag column
      shape = 21, size = 3.5, alpha = 0.5, stroke = 0.5, color = "black"
    ) + # Black border
    scale_fill_viridis_c(option = "plasma", name = "Outlier Probability", limits = c(0, 1)) +
    # Custom colors
    theme_minimal() + # Minimal theme
    labs(x = "Longitude", y = "Latitude", title = "Geographic Map", color = "Flag") + # Labels and title
    theme(
      axis.text = element_text(size = 14), # Increase axis tick label size
      axis.title = element_text(size = 16), # Increase axis label size
      plot.title = element_text(size = 18, hjust = 0.5) # Center and enlarge the title
    )
}
