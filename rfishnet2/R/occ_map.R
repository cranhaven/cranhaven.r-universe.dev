#' Plot Longitude and Latitude Points on World Map
#'
#' \code{occ_map} returns a plot with columns 'Longitude' and 'Latitude' in FishNet2
#' dataframe on a world map.
#'
#' This is a function to get a plot of occurrence records from FishNet2 search query.
#' Parameter is a dataframe that must have the columns 'Longitude' and 'Latitude'.
#' NA values are removed in the function.
#'
#' @export
#' @importFrom grDevices colours
#' @param df A dataframe in FishNet2 standard format (by using read.csv())
#' @param color Color of plotted points, default is dark red
#' @return Plot of latitude and longitude points on world map
#'
#' @examples
#' occ_map(ictaluridae)
occ_map <- function(df, color = "darkred") {

  if(!is.data.frame(df)) {
    stop("Error: Function input is not a dataframe")
  }

  if(!color %in% colours()){
    stop("Error: Color not found")
  }

  if(("Latitude" %in% names(df)) && ("Longitude" %in% names(df))){

  max_lat <- max(df["Latitude"], na.rm = TRUE)
  min_lat <- min(df["Latitude"], na.rm = TRUE)

  max_long <- max(df["Longitude"], na.rm = TRUE)
  min_long <- min(df["Longitude"], na.rm = TRUE)

  return(ggplot2::ggplot(data = world_map) +
    ggplot2::geom_sf() +
    ggplot2::geom_point(data = df, ggplot2::aes(x = Longitude, y = Latitude), size = 2,
               shape = 23, fill=color) +
    ggplot2::coord_sf(xlim = c(min_long - 10, max_long + 10),
             ylim = c(min_lat - 10, max_lat + 10),
             expand = FALSE))
  }
  else{

    stop("Error: Missing columns 'Latitude' and/or 'Longitude'")
  }
}
