#' Visualization of point data.
#'
#' The \code{plot_funds_points} function is designed for visualizing maps of centroids for municipalities using point data. The map colors are determined by the financing amount, and the radius of each point is proportional to the corresponding financing amount.
#'
#' @param data Dataset of class 'data.frame' containing the information about the coordinates of municipalities. Data can be retrieved from all the retrieval functions using the \code{geo_ref = "C"} argument.
#'
#' @param var character. Specify the variable to visualize.
#'
#' @returns Return a \code{leaflet} object representing an interactive map of centroids of muicipalities.
#'
#' @author Lorena Ricciotti
#'
#' @examples data(RENDISpoint)
#' plot_funds_points(RENDISpoint, var = "Finance")
#' #Plotting the points of each municipality of the Basilicata region using the leaflet function.
#'
#' @export
plot_funds_points <- function(data, var) {

  data$geom <- sf::st_as_sfc(data$geom)
  data$Lon <- sf::st_coordinates(data$geom)[,1]
  data$Lat <- sf::st_coordinates(data$geom)[,2]
  # Group data by municipality code and summarize
  summarized_data <- data %>%
    dplyr::group_by(.data$COD_MUNICIPALITY) %>%
    dplyr::summarize(
      tot = sum(get(var), na.rm=T),
      lat = unique(.data$Lat),
      lon = unique(.data$Lon),
      den = unique(.data$DEN_MUNICIPALITY)
    )

  # summarized_data$tot[summarized_data$tot == 0] <- NA
  # Create a color scale based on the log-transformed tot variable
  color_scale <- leaflet::colorNumeric(palette = "viridis",
                                       domain = log(summarized_data$tot+1))
  suppressWarnings(
    # Create a Leaflet map
    map <- leaflet::leaflet(summarized_data) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        lat = ~lat,
        lng = ~lon,
        radius = ~tot^0.3 * 0.1,
        color = ~color_scale(log(tot+1)),
        popup = ~paste("Municipality:",den,"<br> Amount: Euro ", tot)
      ) %>%
      leaflet::addLegend(
        pal = color_scale,
        values = ~log(tot+1),
        title = "Amount",
        opacity = 1,
        labFormat = leaflet::labelFormat(prefix = "\u20AC", transform =
                                           function(tot) {
                                             return(exp(tot) - 1)
                                           })
      )
  )
  print(map)
}
