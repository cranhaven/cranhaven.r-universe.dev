#' Generate a map of summary statistics for a given ARPALdf data.frame
#'
#' @description 'ARPALdf_Summary_map' represents on a map (polygon of Lombardy) the data contained in a data frame
#' of class 'ARPALdf' containing the values or the descriptive statistics by station. Data can be either
#' a ARPALdf of observed data (from 'get_ARPA_Lombardia_xxx' commands) and an ARPALdf obtained as summary descriptive
#' statistic (from 'ARPALdf_Summary' command).
#'
#' @param Data Dataset of class 'ARPALdf' containing the values or the descriptive statistics to plot on the map.
#' Data can be either a ARPALdf of observed data (from 'get_ARPA_Lombardia_xxx' commands) and an ARPALdf obtained
#' as summary descriptive statistic (from 'ARPALdf_Summary' command).
#' @param Title_main Title of the plot.
#' @param Title_legend Title fo the legend
#' @param Variable Summary variable to represent
#' @param prov_line_type Linetype for Lombardy provinces. Default is 1.
#' @param prov_line_size Size of the line for Lombardy provinces. Default is 1.
#' @param col_scale Vector indicating the minimum, the middle and the average point colors.
#' Default is c("green","yellow","red").
#' @param val_midpoint Numeric. Value associated to the middle-point scale color.
#' Default is NULL (midpoint is set equal to the average of the variable to represent).
#' @param xlab x-axis label. Default is 'Longitude'.
#' @param ylab y-axis label. Default is 'Latitude'.
#'
#' @return A map of selected stations across the Lombardy region
#'
#' @examples
#' \donttest{
#' ## Download daily air quality data from all the stations for year 2020
#' if (require("RSocrata")) {
#'     d <- get_ARPA_Lombardia_AQ_data(ID_station = NULL, Date_begin = "2020-01-01",
#'               Date_end = "2020-12-31", Frequency = "daily")
#' }
#' ## Summarising observed data
#' s <- ARPALdf_Summary(Data = d)
#' ## Mapping of the average NO2 in 2020 at several stations
#' ARPALdf_Summary_map(Data = s$Descr_by_IDStat$Mean_by_stat,
#'         Title_main = "Mean NO2 by station in 2020", Variable = "NO2")
#' }
#'
#' @export

ARPALdf_Summary_map <- function(Data, Title_main, Title_legend = "Variable", Variable,
                                prov_line_type = 1, prov_line_size = 1,
                                col_scale = c("#00FF00","#FFFF00","#FF0000"),
                                val_midpoint = NULL,xlab = "Longitude", ylab = "Latitude") {

  ### Checks
  stopifnot("Data is not of class ARPALdf or it does not contains the column IDStation" = is_ARPALdf(Data = Data) == T &
              sum(colnames(Data) == "IDStation") == 1)

  if (is_ARPALdf_AQ_mun(Data = Data) == T) {
    Stats <- get_ARPA_Lombardia_AQ_municipal_registry()
    NUTS_level <- "LAU"
  } else {
    NUTS_level <- "NUTS3"
  }
  Lombardia <- get_Lombardia_geospatial(NUTS_level)
  if (is.null(Lombardia)) {
    message("The map will not include the ground layer with Lombardy's shapefile. Only points/coordinates will be plot.")
  }

  if (is_ARPALdf_AQ(Data = Data) == T) {
    Stats <- get_ARPA_Lombardia_AQ_registry()
  } else if (is_ARPALdf_W(Data = Data) == T) {
    Stats <- get_ARPA_Lombardia_W_registry()
  }


  if (is_ARPALdf_AQ_mun(Data = Data) == T) {
    Data$var <- as.numeric(dplyr::pull(Data[,Variable]))
    ### Scale color midpoint
    if (is.null(val_midpoint)==T) {
      val_midpoint <- mean(Data$var,na.rm=T)
    }
    Data <- dplyr::left_join(Data,Lombardia,by=c("NameStation"="City"))
    Data <- Data %>%
      sf::st_as_sf()

    geo_plot <- Data %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = .data$var)) +
      ggplot2::scale_fill_gradient2(Title_legend, na.value = NA,
                                    low = col_scale[1],
                                    mid = col_scale[2],
                                    midpoint = val_midpoint,
                                    high = col_scale[3]) +
      ggplot2::guides(size = FALSE, scale = "none") +
      ggplot2::labs(title = Title_main) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, '\u00B0', "E")) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, '\u00B0', "N"))

  } else {
    Stats <- Stats %>%
      dplyr::filter(.data$IDStation %in% unique(Data$IDStation)) %>%
      dplyr::distinct(.data$IDStation,.data$Latitude,.data$Longitude)
    Data <- dplyr::left_join(Data,Stats,by=c("IDStation"))
    Data$var <- as.numeric(dplyr::pull(Data[,Variable]))
    ### Scale color midpoint
    if (is.null(val_midpoint)==T) {
      val_midpoint <- mean(Data$var,na.rm=T)
    }
    Data <- Data %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)

    geo_plot <- Lombardia %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(linetype = prov_line_type, size = prov_line_size) +
      ggplot2::geom_sf(data = Data, aes(size = .data$var, col = .data$var)) +
      ggplot2::scale_color_gradient2(Title_legend, na.value = NA,
                                    low = col_scale[1],
                                    mid = col_scale[2],
                                    midpoint = val_midpoint,
                                    high = col_scale[3]) +
      ggplot2::guides(size = FALSE, scale = "none") +
      ggplot2::labs(title = Title_main) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(labels = function(x) paste0(x, '\u00B0', "E")) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, '\u00B0', "N"))
  }

  print(geo_plot)
}
