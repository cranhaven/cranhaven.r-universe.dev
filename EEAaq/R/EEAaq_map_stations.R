#' Create a map representing the stations based on the data given in input or
#' the information specified in the parameters
#'
#' Build static or dynamic maps, representing the location of the stations that detects the specified pollutants.
#' It recieves in input an \code{EEAaq_df} or an \code{EEAaq_df_sfc} class object, or, alternatively, it's possible to specify
#' the required zones and pollutants with the same nomenclature system of the \code{\link{EEAaq_get_data}} function.
#' @param data an \code{EEAaq_df} or \code{EEAaq_df_sfc} class object, which is the output of the \code{\link{EEAaq_get_data}} function.
#' @param pollutant character vector containing the short names of the pollutants for which locate the stations.
#' @param zone_name character vector specifying the names of the zones to consider.
#'                  The reference is the NUTS and LAU nomnclature by Eurostat.
#' @param NUTS_level chracter that specify the level of NUTS or LAU, to which the \code{zone_name} belongs.
#' @param ID logic value (T or F). If \code{TRUE} the character specified in the parameter
#' zone_name is the unique identifier code provided by Eurostat. If \code{FALSE} (the default) it indicates that
#' was specified the full name in latin characters.
#' @param bounds_level character containing the NUTS level or LAU for which draw internal boundaries.
#' Admissible values are "NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU" and it must be of a lower level then
#' the one specified in the parameter \code{NUTS_level}.
#' @param color logical value (T or F). If \code{TRUE} (the default) the points are colored based on the pollutant they are
#' able to detect. If \code{FALSE} the points have the same color.
#' @param dynamic logical value (T or F). If \code{TRUE} the map is interactive and dynamic. If \code{FALSE} (the default) the map is static.
#' @return A map representing the specified area and the points representing the location of the stations able to detect
#' the specified pollutants.
#' @examples
#' \donttest{
#' #Using as example PM data in Lombardia (Italy) during the whole 2022,
#' #it's possible to map the stations in two ways.
#' #First of all specifying the zone information:
#' EEAaq_map_stations(pollutant = c("PM10", "PM2.5"), zone_name = "Lombardia",
#'                    NUTS_level = "NUTS2", ID = FALSE, color = FALSE)
#' #In this case each point have the same color.
#'
#' #Alternatively, it is possible to use the data already downloaded in the parameter data,
#' #coloring the points based on the pollutants the respective station detects.
#' data <- EEAaq_get_data(zone_name = "Milano", NUTS_level = "LAU",
#'   pollutant = "PM10", from = 2023, to = 2023, ID = FALSE, verbose = TRUE)
#' EEAaq_map_stations(data = data, color = TRUE)
#' }
#' @export

EEAaq_map_stations <- function(data = NULL, pollutant = NULL,
                                zone_name = NULL, NUTS_level = NULL, ID = FALSE,
                                bounds_level = NULL, color = TRUE, dynamic = FALSE) {

  `%>%` <- dplyr::`%>%`

  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }


  #Download dei datasets NUTS e LAU
  temp <- tempfile()
  res <- curl::curl_fetch_disk("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/LAU.rds", temp)
  if(res$status_code == 200) {
    LAU <- readRDS(temp)
  } else {
    stop("The internet resource is not available at the moment, try later.
       If the problem persists, please contact the maintainer.")
  }


  temp <- tempfile()
  res <- curl::curl_fetch_disk("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/NUTS.rds", temp)
  if(res$status_code == 200) {
    NUTS <- readRDS(temp)
  } else {
    stop("The internet resource is not available at the moment, try later.
       If the problem persists, please contact the maintainer.")
  }
  stations <- EEAaq_get_stations()

  #Controllo della corretta specificazione dei parametri
  if(is.null(data) & is.null(zone_name)) {
    stop("Either data or zone_name should be specified.")
  } else if(!is.null(data) & !is.null(zone_name)) {
    stop("Either data or zone_name should be specified, not both.")
  }

  "%notin%" <- Negate("%in%")
  #Se viene utilizzato il parametro data, vengono estratti i parametri d'interesse dagli attributi del dataset

  if(!is.null(data) & "EEAaq_df_sfc" %notin% class(data)) {
    zone_name = attributes(data)$zone_name
    NUTS_level = attributes(data)$NUTS_level
    pollutant = attributes(data)$pollutants
  } else if(!is.null(data) & "EEAaq_df_sfc" %in% class(data)) {
    mappa <- attributes(data)$zone_geometry
    NUTS_level = "polygon"
    pollutant = attributes(data)$pollutants
  }

  #Converto il parametro zone_name, nel caso vengano indicati i nomi completi delle zone, nei rispettivi ID
  if(ID == F & NUTS_level == "LAU") {
    zone_name <- LAU %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LAU_NAME %in% zone_name) %>% dplyr::pull(.data$LAU_ID)
  } else if(ID == F & NUTS_level != "NUTS0" & NUTS_level != "polygon") {
    zone_name <- NUTS %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LEVL_CODE == code_extr(NUTS_level) & .data$NAME_LATN %in% zone_name) %>% dplyr::pull(.data$NUTS_ID)
  }


  #NUTS0:
  if(is.null(data)) {
    if(NUTS_level == "NUTS0" & ID == F) {
      if(length(zone_name) == 1) {
        if(zone_name %in% dplyr::pull(dplyr::filter(NUTS, .data$LEVL_CODE == 0), .data$NAME_LATN)) {
          zone_name <- NUTS %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LEVL_CODE == 0 & .data$NAME_LATN %in% zone_name) %>% dplyr::pull(.data$NUTS_ID)
        } else if(zone_name %in% dplyr::pull(dplyr::distinct(stations, .data$Country), .data$Country)) {
          zone_name <- stations %>% dplyr::filter(.data$Country %in% zone_name) %>% dplyr::distinct(.data$ISO) %>% dplyr::pull(.data$ISO)
        } else {
          stop("The specified zone_name is not available")
        }
      } else if(length(zone_name) > 1) {
        zone <- vector()
        for (i in 1:length(zone_name)) {
          if(zone_name[i] %in% dplyr::pull(dplyr::filter(NUTS, .data$LEVL_CODE == 0), .data$NAME_LATN)) {
            zone[i] <- NUTS %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LEVL_CODE == 0 & .data$NAME_LATN %in% zone_name[i]) %>% dplyr::pull(.data$NUTS_ID)
          } else if(zone_name[i] %in% dplyr::pull(dplyr::distinct(stations, .data$Country), .data$Country)) {
            zone[i] <- stations %>% dplyr::filter(.data$Country %in% zone_name[i]) %>% dplyr::distinct(.data$ISO) %>% dplyr::pull(.data$ISO)
          } else {
            warning(paste("The zone", zone_name[i], "is not available"), immediate. = T)
          }
        }
        zone_name <- zone
      }
    } else if(NUTS_level == "NUTS0" & ID == T) {
      if (length(zone_name) == 1) {
        if(zone_name %notin% dplyr::pull(dplyr::distinct(stations, .data$ISO), .data$ISO)) {
          stop("One ore more of the selected zones are not available.")
        }
      } else if(length(zone_name) > 1) {
        if(sum(zone_name %notin% dplyr::pull(dplyr::distinct(stations, .data$ISO), .data$ISO)) == length(zone_name)) {
          stop("All the selected zones are not available.")
        } else if(sum(zone_name %notin% dplyr::pull(dplyr::distinct(stations, .data$ISO), .data$ISO)) <= length(zone_name)) {
          for (i in 1:length(zone_name)) {
            if(zone_name[i] %notin% dplyr::pull(dplyr::distinct(stations, .data$ISO), .data$ISO)) {
              warning(paste("The zone", zone_name[i], "is not available"), immediate. = T)
            }
          }
        }
      }
    }
  }


  #Identifico i bordi piu' esterni da rappresentare in base ai parametri zone_name e NUTS_level
  if(NUTS_level == "LAU") {
    mappa <- dplyr::filter(LAU, .data$LAU_ID %in% zone_name)
  } else if(NUTS_level != "polygon") {
    mappa <- dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% zone_name)
  }

  #Identifico i confini interni che andranno rappresentati in base al parametro bounds_level
  if(dynamic == F) {
    if(!is.null(bounds_level) & NUTS_level != "polygon") {
      if(code_extr(NUTS_level) < code_extr(bounds_level)) {
        if(bounds_level == "LAU") {
          NUTS_LAU <- sf::st_join(dplyr::filter(LAU, .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
          bounds <- dplyr::filter(NUTS_LAU, .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% zone_name)
        } else {
          bounds <- NUTS %>% sf::st_as_sf() %>% dplyr::filter(.data$LEVL_CODE == code_extr(bounds_level) & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
          #bounds <- NUTS[NUTS$LEVL_CODE == code_extr(bounds_level) & substr(NUTS$NUTS_ID, 1, code_extr(NUTS_level)+2) %in% mappa$NUTS_ID, ]
        }
      } else if(code_extr(NUTS_level) >= code_extr(bounds_level)) {
        warning("The parameter bounds_level should be of a lower order then the parameter NUTS_level")
        bounds <- NULL
      }
    } else if(NUTS_level == "polygon" & !is.null(bounds_level)) {
      if(bounds_level == "LAU") {
        ind <- sf::st_intersects(LAU, mappa, sparse = T)
        bounds <- LAU[as.logical(apply(as.matrix(ind), 1, sum)),]
      } else {
        ind <- sf::st_intersects(dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(bounds_level)), mappa, sparse = T)
        bounds <- dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(bounds_level))[as.logical(apply(as.matrix(ind), 1, sum)),]
      }
    } else {bounds <- NULL}
  }


  #Filtro le centraline da rappresentare in base agli inquinanti specificati
  if(!is.null(pollutant) & is.null(data)) {
    points <- stations %>% dplyr::filter(.data$AirPollutant %in% pollutant) %>%
      dplyr::distinct(.data$Country, .data$ISO, .data$AirQualityStationEoICode, .data$AirQualityStationNatCode,
                      .data$AirQualityStationName, .data$Longitude, .data$Latitude, .data$Altitude, .data$NUTS1,
                      .data$NUTS1_ID, .data$NUTS2, .data$NUTS2_ID, .data$NUTS3, .data$NUTS3_ID, .data$LAU, .data$LAU_ID, .data$AirPollutant)
  } else if(is.null(pollutant) & is.null(data)){
    points <- stations %>%
      dplyr::distinct(.data$Country, .data$ISO, .data$AirQualityStationEoICode, .data$AirQualityStationNatCode,
                      .data$AirQualityStationName, .data$Longitude, .data$Latitude, .data$Altitude, .data$NUTS1,
                      .data$NUTS1_ID, .data$NUTS2, .data$NUTS2_ID, .data$NUTS3, .data$NUTS3_ID, .data$LAU, .data$LAU_ID, .data$AirPollutant)
  } else if(!is.null(data)) {
    points <- stations %>% dplyr::filter(.data$AirQualityStationEoICode %in% unique(data$AirQualityStationEoICode)) %>% dplyr::filter(.data$AirPollutant %in% pollutant) %>%
      dplyr::distinct(.data$Country, .data$ISO, .data$AirQualityStationEoICode, .data$AirQualityStationNatCode,
                      .data$AirQualityStationName, .data$Longitude, .data$Latitude, .data$Altitude, .data$NUTS1,
                      .data$NUTS1_ID, .data$NUTS2, .data$NUTS2_ID, .data$NUTS3, .data$NUTS3_ID, .data$LAU, .data$LAU_ID, .data$AirPollutant)
  }

  #Se la stessa centralina rileva piu' inquinanti, le raggruppo in un unico punto
  if(!is.null(pollutant) & length(pollutant) > 1) {
    points <- points %>% tidyr::pivot_wider(names_from = "AirPollutant", values_from = "AirPollutant") %>% tidyr::unite(col = "AirPollutant", pollutant, sep = ", ", na.rm = TRUE)
  }


  #Filtraggo delle centraline da rappresentare in base alle zone richieste
  if(is.null(data)){
    if(NUTS_level == "LAU") {
      points <- dplyr::filter(points, .data$LAU_ID %in% zone_name)
    } else if(NUTS_level == "NUTS0") {
      points <- dplyr::filter(points, .data$ISO %in% zone_name)
    } else if(NUTS_level == "NUTS1") {
      points <- dplyr::filter(points, .data$NUTS1_ID %in% zone_name)
    } else if(NUTS_level == "NUTS2") {
      points <- dplyr::filter(points, .data$NUTS2_ID %in% zone_name)
    } else if(NUTS_level == "NUTS3") {
      points <- dplyr::filter(points, .data$NUTS3_ID %in% zone_name)
    }
  }

  #Errore nel caso non ci siano centraline per i parametri specificati:
  if(nrow(points) == 0){
    stop("There is no station available in the specified zone for these pollutants")
  }

  #Aggiungo le geometrie dei punti
  points <-  sf::st_as_sf(stats::na.omit(points), coords = c("Longitude", "Latitude"), crs = 4326)


  #Costruzione della mappa da rappresentare
  if(dynamic == F) {
    if(!is.null(bounds)) {
      build_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = bounds, fill = NA, lwd = .2, col = "black") +
        ggplot2::geom_sf(data = mappa, fill = NA, lwd = .4, col = "black") +
        switch(as.character(color),
               "TRUE" = {ggplot2::geom_sf(data = points, ggplot2::aes(col = .data$AirPollutant))},
               "FALSE" = {ggplot2::geom_sf(data = points, col = "black")}
        ) +
        ggplot2::labs(x = "Longitude", y = "Latitude") +
        #ggspatial::annotation_north_arrow(which_north = "true", height = ggplot2::unit(.7, "cm"), width = ggplot2::unit(.7, "cm")) +
        #ggspatial::annotation_scale(location="br") +
        ggplot2::theme_bw()
    } else {
      build_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = mappa, fill = NA, lwd = .4, col = "black") +
        switch(as.character(color),
               "TRUE" = {ggplot2::geom_sf(data = points, ggplot2::aes(col = .data$AirPollutant))},
               "FALSE" = {ggplot2::geom_sf(data = points, col = "black")}
        ) +
        ggplot2::labs(x = "Longitude", y = "Latitude") +
        #ggspatial::annotation_north_arrow(which_north = "true", height = ggplot2::unit(.7, "cm"), width = ggplot2::unit(.7, "cm")) +
        #ggspatial::annotation_scale(location="br") +
        ggplot2::theme_bw()
    }
  } else if(dynamic == T) {
    mypal = leaflet::colorFactor(palette = grDevices::rainbow(n = length(unique(points$AirPollutant))), domain = points$AirPollutant)
    if(NUTS_level == "polygon") {
      build_map <- leaflet::leaflet(mappa) %>% leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap,group = "Esri.WorldStreetMap") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>% leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
        leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = F)) %>%
        leaflet::addMapPane("polygons", zIndex = 410) %>%
        leaflet::addMapPane("circles", zIndex = 420) %>%
        leaflet::addPolygons(color = "black",  weight = 2.5, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                    highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                    options = leaflet::pathOptions(pane = "polygons"))
      if(!is.null(bounds_level)) {
        if(bounds_level == "NUTS0") {
          ind <- sf::st_intersects(NUTS, mappa, sparse = T)
          mappa_nuts <- NUTS[as.logical(apply(as.matrix(ind), 1, sum)),]
          mappa_nuts0 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 0)
          mappa_nuts1 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 1)
          mappa_nuts2 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 2)
          mappa_nuts3 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 3)
          ind <- sf::st_intersects(LAU, mappa, sparse = T)
          mappa_lau <- LAU[as.logical(apply(as.matrix(ind), 1, sum)),]
          build_map <- build_map %>% leaflet::addPolygons(data = mappa_nuts0,color = "black",  weight = 2, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts0$NAME_LATN, group = "NUTS 0",
                                                 options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_nuts1, group = "NUTS 1", color = "black",  weight = 1.5,
                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                        popup = mappa_nuts1$NAME_LATN, options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1.2,
                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts2$NAME_LATN,
                        options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = .5,
                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                        options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                        options = leaflet::pathOptions(pane = "polygons"))
        } else if(bounds_level == "NUTS1") {
          ind <- sf::st_intersects(NUTS, mappa, sparse = T)
          mappa_nuts <- NUTS[as.logical(apply(as.matrix(ind), 1, sum)),]
          mappa_nuts1 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 1)
          mappa_nuts2 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 2)
          mappa_nuts3 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 3)
          ind <- sf::st_intersects(LAU, mappa, sparse = T)
          mappa_lau <- LAU[as.logical(apply(as.matrix(ind), 1, sum)),]
          build_map <- build_map %>% leaflet::addPolygons(data = mappa_nuts1, group = "NUTS 1", color = "black",  weight = 1.5,
                                                 smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                                 popup = mappa_nuts1$NAME_LATN, options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1.2,
                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts2$NAME_LATN,
                        options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = .5,
                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                        options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                        options = leaflet::pathOptions(pane = "polygons"))
        } else if(bounds_level == "NUTS2") {
          ind <- sf::st_intersects(NUTS, mappa, sparse = T)
          mappa_nuts <- NUTS[as.logical(apply(as.matrix(ind), 1, sum)),]
          mappa_nuts2 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 2)
          mappa_nuts3 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 3)
          ind <- sf::st_intersects(LAU, mappa, sparse = T)
          mappa_lau <- LAU[as.logical(apply(as.matrix(ind), 1, sum)),]
          build_map <- build_map %>% leaflet::addPolygons(data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1.2,
                                                 smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts2$NAME_LATN,
                                                 options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = .5,
                        smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                        options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                        options = leaflet::pathOptions(pane = "polygons"))
        } else if(bounds_level == "NUTS3") {
          ind <- sf::st_intersects(NUTS, mappa, sparse = T)
          mappa_nuts <- NUTS[as.logical(apply(as.matrix(ind), 1, sum)),]
          mappa_nuts3 <- dplyr::filter(mappa_nuts, .data$LEVL_CODE == 3)
          ind <- sf::st_intersects(LAU, mappa, sparse = T)
          mappa_lau <- LAU[as.logical(apply(as.matrix(ind), 1, sum)),]
          build_map <- build_map %>% leaflet::addPolygons(data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = .5,
                                                 smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                                                 options = leaflet::pathOptions(pane = "polygons")) %>%
            leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                        smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                        highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                        options = leaflet::pathOptions(pane = "polygons"))
        } else if(bounds_level == "LAU"){
          ind <- sf::st_intersects(LAU, mappa, sparse = T)
          mappa_lau <- LAU[as.logical(apply(as.matrix(ind), 1, sum)),]
          build_map <- build_map %>% leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                                                 smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                                                 options = leaflet::pathOptions(pane = "polygons"))
        }
        lays <- function(x) {
          vec <- c("NUTS 0", "NUTS 1", "NUTS 2", "NUTS 3", "LAU")
          ind <- code_extr(x) + 1
          res <- vec[ind:length(vec)]
          return(res)
        }
        build_map <- build_map %>% leaflet::addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
                                                    position = "topleft" , overlayGroups = lays(bounds_level), options = leaflet::layersControlOptions(collapsed = TRUE))

      } else {
        build_map <- build_map %>% leaflet::addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
                                                    position = "topleft", options = leaflet::layersControlOptions(collapsed = TRUE))
      }

    } else if(NUTS_level != "polygon") {
      build_map <- leaflet::leaflet(mappa) %>% leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap,group = "Esri.WorldStreetMap") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
        leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = F)) %>%
        leaflet::addMapPane("polygons", zIndex = 410) %>%
        leaflet::addMapPane("circles", zIndex = 420) %>%
        leaflet::addPolygons(color = "black", group = ifelse(NUTS_level != "LAU", paste("NUTS", code_extr(NUTS_level)), "LAU"),  weight = 2.5, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                    highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                    options = leaflet::pathOptions(pane = "polygons"))

      if(NUTS_level == "NUTS0") {
        mappa_nuts1 <- dplyr::filter(NUTS, .data$LEVL_CODE == 1 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
        mappa_nuts2 <- dplyr::filter(NUTS, .data$LEVL_CODE == 2 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
        mappa_nuts3 <- dplyr::filter(NUTS, .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
        NUTS_LAU <- sf::st_join(dplyr::filter(LAU, .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
        mappa_lau <- dplyr::filter(NUTS_LAU, .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
        build_map <- leaflet::addPolygons(map = build_map, data = mappa_nuts1, group = "NUTS 1", color = "black",  weight = 1.5,
                                 smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                 popup = mappa_nuts1$NAME_LATN, options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addPolygons(data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1.2,
                      smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts2$NAME_LATN,
                      options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addPolygons(data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = .5,
                      smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                      options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                      smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                      options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
                           position = "topleft" , overlayGroups = c("NUTS 0", "NUTS 1", "NUTS 2", "NUTS 3", "LAU"), options = leaflet::layersControlOptions(collapsed = TRUE))
      } else if(NUTS_level == "NUTS1") {
        mappa_nuts2 <- dplyr::filter(NUTS, .data$LEVL_CODE == 2 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
        mappa_nuts3 <- dplyr::filter(NUTS, .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
        NUTS_LAU <- sf::st_join(dplyr::filter(LAU, .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
        mappa_lau <- dplyr::filter(NUTS_LAU, .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
        build_map <- leaflet::addPolygons(map = build_map, data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1.2,
                                 smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts2$NAME_LATN,
                                 options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addPolygons(data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = .5,
                      smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                      options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                      smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                      options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
                           position = "topleft" , overlayGroups = c("NUTS 1", "NUTS 2", "NUTS 3", "LAU"), options = leaflet::layersControlOptions(collapsed = TRUE))
      } else if(NUTS_level == "NUTS2") {
        mappa_nuts3 <- dplyr::filter(NUTS, .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
        NUTS_LAU <- sf::st_join(dplyr::filter(LAU, .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
        mappa_lau <- dplyr::filter(NUTS_LAU, .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
        build_map <- leaflet::addPolygons(map = build_map, data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = 1,
                                 smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                                 options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .5,
                      smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                      options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
                           position = "topleft" , overlayGroups = c("NUTS 2", "NUTS 3", "LAU"), options = leaflet::layersControlOptions(collapsed = TRUE))
      } else if(NUTS_level == "NUTS3") {
        NUTS_LAU <- sf::st_join(dplyr::filter(LAU, .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(NUTS, .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
        mappa_lau <- dplyr::filter(NUTS_LAU, .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
        build_map <- leaflet::addPolygons(map = build_map, data = mappa_lau, group = "LAU", color = "black",  weight = .5,
                                 smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                                 highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                                 options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
                           position = "topleft" , overlayGroups = c("NUTS 3", "LAU"), options = leaflet::layersControlOptions(collapsed = TRUE))
      } else if(NUTS_level == "LAU") {
        build_map <- leaflet::addLayersControl(map = build_map, baseGroups = c("Esri.WorldGrayCanvas", "Esri.WorldStreetMap", "Esri.WorldTopoMap","Esri.WorldImagery"),
                                      position = "topleft" , overlayGroups = c("LAU"), options = leaflet::layersControlOptions(collapsed = TRUE))
      }


    }
    build_map <- leaflet::addCircleMarkers(map = build_map, data = points,  label = ~AirQualityStationEoICode,  fillColor = switch(as.character(color), "TRUE" = {~mypal(points$AirPollutant)}, "FALSE" = {"black"}), fillOpacity = 1, radius = 4, stroke = F,
                                           popup = paste("Air Quality Station EoI Code:", points$AirQualityStationEoICode, "<br>", "Air Quality Station National Code:", points$AirQualityStationNatCode, "<br>",  "Air Quality Station Name:", points$AirQualityStationName,"<br>","Country:", points$ISO, "<br>","NUTS 1:", points$NUTS1,
                                                         "<br>", "NUTS 2:", points$NUTS2, "<br>", "NUTS 3:", points$NUTS3, "<br>", "LAU:", points$LAU),
                                           labelOptions = leaflet::labelOptions(bringToFront = T), options = leaflet::pathOptions(pane = "circles"))
    build_map <- switch(as.character(color), "TRUE" = {leaflet::addLegend(map = build_map, position = "topright", pal = mypal, data = points, values = points$AirPollutant)}, "FALSE" = {build_map})
  }
  return(build_map)
}






