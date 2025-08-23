#' Visual representation by mapping municipalities' polygons and color-coding them according to financial expenditures.
#'
#' The \code{plot_funds_map} function is designed for visualizing areal data within a region. It generates an informative map where each municipality is represented with a unique color determined by its corresponding financing amount.
#'
#'
#' @param data  dataset of class 'data.frame'.
#' Specify the dataset from which to take information. The dataset must contain the geometry of the polygons of each municipality.
#'
#' @param var character. Specify the name of the variable to visualize.
#' @returns Return \code{ggplot} object representing an interactive map.
#'
#' @author Lorena Ricciotti
#'
#' @examples
#' #Retrieve data with the polygons of the municipalities
#' \donttest{RENDISarea <- get_data_RENDIS("12", geo_ref = "A")
#' plot_funds_map(RENDISarea, var = "Finance")}
#'
#' #Plotting the map for Lazio region to visualize the total public
#' #expenditure divided by municipality.
#'
#' @export
plot_funds_map <- function(data, var) {

  data <- data %>%
    dplyr::group_by(.data$COD_MUNICIPALITY) %>%
    dplyr::reframe(tot = sum(get(var), na.rm = T),
                   geom = unique(.data$geom),
                   den = unique(.data$DEN_MUNICIPALITY))
  data$geom <- sf::st_as_sfc(data$geom)
  #data$geom <- do.call(st_sfc, data$geom)

  dati <- data %>%
    dplyr::filter(!rlang::is_empty(.data$geom))
  #Log scale
  #dati$log_tot <- log(dati$tot)
  dati$geom <- do.call(sf::st_sfc, dati$geom)
  dati$geom <- sf::st_cast(dati$geom, "MULTIPOLYGON")
  dati <- as.data.frame(dati)
  suppressWarnings(

    p <- ggplot2::ggplot(data = dati) +
      ggplot2::geom_sf( ggplot2::aes(geometry = .data$geom, fill = .data$tot, text = gsub("\n", " ", paste("Municipality: ", .data$den,  "\n Tot:",.data$tot, "Euro")))) +
      ggplot2::scale_fill_viridis_c(trans = "log") +
      ggplot2::labs(fill = "Total funds (Euro)", title = "Total Funds (log-scale)")+
      ggplot2::theme_bw()
  )

  suppressWarnings(print(p))


}




