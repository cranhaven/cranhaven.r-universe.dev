#' Create a static or dynamic (interactive leaflet) map representing the geographical locations of the
#' stations based on a user-defined input dataset of class \code{EEAaq_df} or \code{EEAaq_df_sfc}.
#'
#' @param data an \code{EEAaq_df} or \code{EEAaq_df_sfc} class object, which is the output of the \code{\link{EEAaq_get_data}} function.
#' @param NUTS_extborder character containing the NUTS level or LAU for which draw external boundaries.
#' Admissible values are 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' Recall that the NUTS classification (Nomenclature of territorial units for statistics) is a hierarchical system for dividing up the economic territory of the EU and the UK.
#' The levels are defined as follows:
#' \itemize{
#' \item{\strong{NUTS 0}: the whole country}
#' \item{\strong{NUTS 1}: major socio-economic regions}
#' \item{\strong{NUTS 2}: basic regions for the application of regional policies}
#' \item{\strong{NUTS 3}: small regions for specific diagnoses}
#' \item{\strong{LAU}: municipality}
#' }
#' @param NUTS_intborder character containing the NUTS level or LAU for which draw internal boundaries.
#' Admissible values are 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' @param color logical value (T or F). If \code{TRUE} (the default) the points are colored based on the pollutant they are
#' able to detect. If \code{FALSE} the points have the same color.
#' @param dynamic logical value (T or F). If \code{TRUE} the map is interactive and dynamic. If \code{FALSE} (the default) the map is static.
#' @return A map representing the specified area and the points representing the location of the stations able to detect
#' the specified pollutants.
#' @examples
#' \donttest{
#' library(sf)
#' `%>%` <- dplyr::`%>%`
#' ### Retrieve all the stations measuring PM10 in Belgium
#' IDstations <- EEAaq_get_stations(byStation = FALSE, complete = TRUE)
#' IDstations <- IDstations %>%
#'   dplyr::filter(ISO %in% c("BE"),
#'                 is.na(OperationalActivityEnd),
#'                 AirPollutant %in% "PM10") %>%
#'   dplyr::pull(AirQualityStationEoICode) %>%
#'   unique()
#'
#' ### Download the corresponding data from December 1st to December 31st, 2021
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
#'                        from = "2021-12-01", to = "2021-12-31",
#'                        verbose = TRUE)
#'
#' ### Static map of available stations across the whole country.
#' ### External borders are given by the union of the available regions (NUTS-2),
#' ###   while municipalities (LAUs) are used as inner borders.
#' EEAaq_map_stations(data = data,
#'                    NUTS_extborder = "NUTS1", NUTS_intborder = "NUTS2",
#'                    color = TRUE, dynamic = FALSE)
#' ### Dynamic (interactive leaflet) map of available stations across the whole
#' ###  country. External borders are given by the union of the available
#' ###  regions (NUTS-2), while provinces (NUTS-3) are used as inner borders.
#' EEAaq_map_stations(data = data,
#'                    NUTS_extborder = "NUTS2", NUTS_intborder = "NUTS3",
#'                    color = TRUE, dynamic = TRUE)
#' }
#'
#' @export

EEAaq_map_stations <- function(data = NULL, NUTS_extborder = NULL, NUTS_intborder = NULL,
                               color = TRUE, dynamic = FALSE) {

  ############################
  ##### Setup and checks #####
  ############################
  "%notin%" <- Negate("%in%")
  `%>%` <- dplyr::`%>%`

  # Check internet connection
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }

  # Controllo della corretta specificazione dei parametri
  if(is.null(data)) {
    stop("Data should be specified.")
  }

  # Check on "NUTS_"
  if(!is.null(NUTS_extborder)) {
    stopifnot("The parameter NUTS_extborder should be one of NUTS0, NUTS1, NUTS2, NUTS3, LAU" = NUTS_extborder %in% c("LAU", "NUTS0", "NUTS1", "NUTS2", "NUTS3"))
  }
  if(!is.null(NUTS_intborder)) {
    stopifnot("The parameter NUTS_intborder should be one of NUTS0, NUTS1, NUTS2, NUTS3, LAU" = NUTS_intborder %in% c("LAU", "NUTS0", "NUTS1", "NUTS2", "NUTS3"))
  }


  ##### Download stations metadata
  stations <- EEAaq_get_dataframe(dataframe = "stations")

  ##### Filter only stations of interest
  filter_stations <- stations %>%
    dplyr::filter(.data$AirQualityStationEoICode %in% unique(data$AirQualityStationEoICode))

  ##### Define maximum area of interest (AOI) according to the external border
  AOI <- unique(filter_stations[[paste0("NUTS",code_extr(NUTS_extborder),"_ID")]])

  ##### Retrieve and filter metadata
  pollutants <- EEAaq_get_dataframe(dataframe = "pollutant")
  LAU <- EEAaq_get_dataframe(dataframe = "LAU") %>%
    dplyr::rename(geometry = .data$Lau_geometry) %>%
    dplyr::filter(.data$ISO %in% unique(filter_stations$ISO),
                  grepl(pattern = paste(AOI,sep = "",collapse = "|"), x = .data$NUTS3_ID))
  NUTS <- EEAaq_get_dataframe(dataframe = "NUTS") %>%
    dplyr::filter(.data$LEVL_CODE %in% c(code_extr(NUTS_extborder),
                                         code_extr(NUTS_intborder)),
                  grepl(pattern = paste(AOI,sep = "",collapse = "|"), x = .data$NUTS_ID),
                  .data$CNTR_CODE %in% unique(filter_stations$ISO))

  ##### Extract geometries of the AOI
  if (NUTS_extborder == "LAU") {
    mappa <- LAU %>%
      dplyr::filter(.data$LAU_ID %in% unique(filter_stations$LAU_ID),
                    .data$ISO %in% unique(filter_stations$ISO)) %>%
      sf::st_as_sf()
    zone_name <- mappa %>%
      sf::st_drop_geometry() %>%
      dplyr::pull(.data$LAU_ID)
  } else if (NUTS_extborder == "NUTS0") {
    mappa <- NUTS %>%
      dplyr::filter(.data$LEVL_CODE == 0,
                    .data$NUTS_ID %in% unique(filter_stations$NUTS0_ID)) %>%
      sf::st_as_sf()
    zone_name <- mappa %>%
      sf::st_drop_geometry() %>%
      dplyr::pull(.data$NUTS_ID)
  } else if (NUTS_extborder == "NUTS1") {
    mappa <- NUTS %>%
      dplyr::filter(.data$LEVL_CODE == 1,
                    .data$NUTS_ID %in% unique(filter_stations$NUTS1_ID)) %>%
      sf::st_as_sf()
    zone_name <- mappa %>%
      sf::st_drop_geometry() %>%
      dplyr::pull(.data$NUTS_ID)
  } else if (NUTS_extborder == "NUTS2") {
    mappa <- NUTS %>%
      dplyr::filter(.data$LEVL_CODE == 2,
                    .data$NUTS_ID %in% unique(filter_stations$NUTS2_ID)) %>%
      sf::st_as_sf()
    zone_name <- mappa %>%
      sf::st_drop_geometry() %>%
      dplyr::pull(.data$NUTS_ID)
  } else if (NUTS_extborder == "NUTS3") {
    mappa <- NUTS %>%
      dplyr::filter(.data$LEVL_CODE == 3,
                    .data$NUTS_ID %in% unique(filter_stations$NUTS3_ID)) %>%
      sf::st_as_sf()
    zone_name <- mappa %>%
      sf::st_drop_geometry() %>%
      dplyr::pull(.data$NUTS_ID)
  }


  ######################################################
  ##### Data management of stations in the dataset #####
  ######################################################
  points <- stations %>%
    dplyr::filter(.data$AirQualityStationEoICode %in% unique(data$AirQualityStationEoICode)) %>%
    dplyr::distinct(.data$Country, .data$ISO, .data$AirQualityStationEoICode, .data$AirQualityStationNatCode,
                    .data$AirQualityStationName, .data$Longitude, .data$Latitude, .data$Altitude, .data$NUTS1,
                    .data$NUTS1_ID, .data$NUTS2, .data$NUTS2_ID, .data$NUTS3, .data$NUTS3_ID, .data$LAU_NAME, .data$LAU_ID, .data$AirPollutant)
  polls <- unique(points$AirPollutant)
  # Se la stessa centralina rileva piu' inquinanti, le raggruppo in un unico punto
  points <- points %>%
    tidyr::pivot_wider(names_from = "AirPollutant", values_from = "AirPollutant") %>%
    tidyr::unite(col = "AirPollutant", tidyselect::all_of(polls), sep = ", ", na.rm = TRUE)
  # Aggiungo le geometrie dei punti
  points <-  sf::st_as_sf(stats::na.omit(points), coords = c("Longitude", "Latitude"), crs = 4326)




  if(dynamic == F) {
    ######################
    ##### Static map #####
    ######################
    if(NUTS_intborder == "NUTS0") {
      mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 0)
    } else if(NUTS_intborder == "NUTS1") {
      mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 1)
    } else if(NUTS_intborder == "NUTS2") {
      mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 2)
    } else if(NUTS_intborder == "NUTS3") {
      mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 3)
    } else if(NUTS_intborder == "LAU") {
      mappa_internal <- LAU
    }

    build_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = mappa, fill = NA, lwd = .4, col = "black") +
      ggplot2::geom_sf(data = mappa_internal, fill = NA, lwd = .7) +
      switch(as.character(color),
             "TRUE" = {
               ggplot2::geom_sf(data = points, ggplot2::aes(col = .data$AirPollutant))
             },
             "FALSE" = {
               ggplot2::geom_sf(data = points, col = "black")
             }
      ) +
      ggplot2::labs(x = "Longitude", y = "Latitude") +
      ggspatial::annotation_north_arrow(which_north = "true", height = ggplot2::unit(.7, "cm"), width = ggplot2::unit(.7, "cm")) +
      ggspatial::annotation_scale(location="br") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
  } else if(dynamic == T) {

    #######################
    ##### Dynamic map #####
    #######################
    mypal <- leaflet::colorFactor(palette = grDevices::rainbow(n = length(unique(points$AirPollutant))), domain = points$AirPollutant)

    ##### Baseline map
    build_map <- leaflet::leaflet(mappa) %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap,group = "Esri.WorldStreetMap") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = F)) %>%
      leaflet::addMapPane("polygons", zIndex = 410) %>%
      leaflet::addMapPane("circles", zIndex = 420) %>%
      leaflet::addPolygons(color = "black",  weight = 2.5, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                           highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                           options = leaflet::pathOptions(pane = "polygons"))

    ##### Add external and internal borders
    if(NUTS_intborder == "NUTS0") {
      mappa_nuts0 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 0 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      build_map <- leaflet::addPolygons(map = build_map, data = mappa_nuts0, group = "NUTS 0", color = "black",  weight = 1,
                                        label = ~ paste0("NUTS_ID: ",NUTS_ID," -- ",NUTS_NAME),
                                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                        popup = paste0(
                                          "Country: ",mappa_nuts0$CNTR_CODE,
                                          "<br>","NUTS level: 0",
                                          "<br>","Area ID: ",mappa_nuts0$NAME_ID,
                                          "<br>","Area Name: ",mappa_nuts0$NAME_LATN
                                        ),
                                        options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_intborder == "NUTS1") {
      mappa_nuts1 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 1 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      build_map <- leaflet::addPolygons(map = build_map, data = mappa_nuts1, group = "NUTS 1", color = "black",  weight = 1,
                                        label = ~ paste0("NUTS_ID: ",NUTS_ID," -- ",NUTS_NAME),
                                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                        popup = paste0(
                                          "Country: ",mappa_nuts1$CNTR_CODE,
                                          "<br>","NUTS level: 1",
                                          "<br>","Area ID: ",mappa_nuts1$NAME_ID,
                                          "<br>","Area Name: ",mappa_nuts1$NAME_LATN
                                        ),
                                        options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_intborder == "NUTS2") {
      mappa_nuts2 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 2 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      build_map <- leaflet::addPolygons(map = build_map, data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1,
                                        label = ~ paste0("NUTS_ID: ",NUTS_ID," -- ",NUTS_NAME),
                                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                        popup = paste0(
                                          "Country: ",mappa_nuts2$CNTR_CODE,
                                          "<br>","NUTS level: 2",
                                          "<br>","Area ID: ",mappa_nuts2$NAME_ID,
                                          "<br>","Area Name: ",mappa_nuts2$NAME_LATN
                                        ),
                                        options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_intborder == "NUTS3") {
      mappa_nuts3 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      build_map <- leaflet::addPolygons(map = build_map, data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = 1,
                                        label = ~ paste0("NUTS_ID: ",NUTS_ID," -- ",NUTS_NAME),
                                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                        popup = paste0(
                                          "Country: ",mappa_nuts3$CNTR_CODE,
                                          "<br>","NUTS level: 3",
                                          "<br>","Area ID: ",mappa_nuts3$NAME_ID,
                                          "<br>","Area Name: ",mappa_nuts3$NAME_LATN
                                        ),
                                        options = leaflet::pathOptions(pane = "polygons")
      )
    } else if(NUTS_intborder == "LAU") {
      NUTS_LAU <- sf::st_join(x = sf::st_as_sf(LAU),
                              y = dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_extborder)),
                              largest = T)
      mappa_lau <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_extborder) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
      build_map <- leaflet::addPolygons(map = build_map, data = mappa_lau,
                                        label = ~ paste0("NUTS_ID: ",NUTS_ID," -- ",NUTS_NAME),
                                        group = "LAU", color = "black",  weight = .5,
                                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                        popup = paste0(
                                          "Country: ",mappa_lau$CNTR_CODE,
                                          "<br>","LAU ID: ",mappa_lau$LAU_ID,
                                          "<br>","LAU Name: ",mappa_lau$LAU_NAME
                                        ),
                                        options = leaflet::pathOptions(pane = "polygons"))
    }

    ##### Add stations coordinates
    build_map <- leaflet::addCircleMarkers(
      map = build_map,
      data = points,
      label = ~ paste0("EoICode: ", AirQualityStationEoICode," -- ",AirPollutant),
      fillColor = switch(
        as.character(color),
        "TRUE" = {
          ~ mypal(points$AirPollutant)
        },
        "FALSE" = {"black"}
      ),
      fillOpacity = 1, radius = 4, stroke = F,
      popup = paste("Air Quality Station EoI Code:", points$AirQualityStationEoICode, "<br>", "Air Quality Station National Code:", points$AirQualityStationNatCode, "<br>",  "Air Quality Station Name:", points$AirQualityStationName,
                    "<br>","Pollutants:", points$AirPollutant,
                    "<br>","Country:", points$ISO, "<br>","NUTS 1:", points$NUTS1,
                    "<br>", "NUTS 2:", points$NUTS2, "<br>", "NUTS 3:", points$NUTS3, "<br>", "LAU:", points$LAU_NAME),
      labelOptions = leaflet::labelOptions(bringToFront = T),
      options = leaflet::pathOptions(pane = "circles")
    )

    ##### Add legend
    build_map <- switch(as.character(color),
                        "TRUE" = {
                          leaflet::addLegend(map = build_map, position = "topright", pal = mypal, data = points, values = points$AirPollutant)
                        },
                        "FALSE" = {build_map}
    )
    # leaflet::addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
    #                           position = "topleft" , overlayGroups = c("NUTS 0", "NUTS 1", "NUTS 2", "NUTS 3", "LAU"), options = leaflet::layersControlOptions(collapsed = TRUE))

  }

  return(build_map)
}


