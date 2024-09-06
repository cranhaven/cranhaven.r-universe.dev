#' Build a spatial interpolation map based on the Inverse Distance Weighting technique.
#'
#' \code{EEAaq_idw_map} recieves as input a \code{EEAaq_taggr_df} or a \code{EEAaq_taggr_df_sfc} class object and produces a
#' spatial interpolation map. Depending on the time frequency of the aggregation, multiple maps are generated, one for
#' each timestamp.
#' It may be exported as pdf, jpeg, png, gif and html.
#' @param data an object of class \code{EEAaq_taggr_df} or \code{EEAaq_taggr_df_sfc}, which is the
#' output of the \code{\link{EEAaq_time_aggregate}} function.
#' @param pollutant vector containing the pollutant for which to build the map. It must be one of the pollutants
#' contained in \code{data}.
#' @param aggr_fun charachter containing the aggregation function to use for computing the interpolation. It must
#' be one of the statistics contained in \code{data}.
#' @param bounds_level character containing the NUTS level or LAU for which draw internal boundaries.
#' Admissible values are 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' @param distinct logic value (T or F). If \code{TRUE}, each map generated is printed and saved in distinct pages
#' (for instance if \code{data} has a monthly frequency in a yearly time window, 12 distinct plots are generated).
#' If \code{FALSE} (the default), the maps are printed in a single page.
#' @param gradient logic value (T or F). If \code{TRUE} (the default) the maps generated are colored with a
#' continuous color scale. If \code{FALSE}, the color scale is discrete.
#' @param idp numeric value that specify the inverse distance weighting power. For further information see
#' \code{\link{idw}}.
#' @param nmax numeric value; specify the number of nearest observations that should be
#' used for the inverse distance weighting computing, where nearest is defined in terms of the
#' space of the spatial locations. By default, all observations are used. For further information see
#' \code{\link{idw}}
#' @param maxdist numeric value; only observations within a distance of \code{maxdist} from the prediction location
#' are used for the idw computation. By default, all observations are used.
#' If combined with \code{nmax}, both criteria apply.
#' @param dynamic logic value (T or F). If \code{TRUE} the function creates a Leaflet map widget using
#' \bold{htmlwidgets} (for further information see \code{\link{leaflet}}). If \code{FALSE} (the default)
#' the maps generated are static.
#' @param fill_NUTS_level character containing the NUTS level or LAU for which to aggregate the idw computing,
#' in order to obtain a uniform coloring inside each area at the specified level.
#' (For instance if \code{fill_NUTS_level = "LAU"}, each municipality is filled by the mean value of the pollutant
#' concentration, computed by the idw, of each pixel inside the respective municipality). Allowed values are
#' 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' @param tile character representing the name of the provider tile. To see the full list of the providers, run
#' \code{leaflet::providers}. For further information see \code{\link{addProviderTiles}}.
#' @param save character representing in which extension to save the map. Allowed values are 'jpeg', 'png', 'pdf'
#' (if \code{dynamic = FALSE}), 'gif' (if \code{dynamic = FALSE & distinct = TRUE}), 'html' (if \code{dynamic = TRUE}).
#' @param filepath a character string giving the file path.
#' @param width,height the width and the height of the plot, expressed in pixels (by default \code{width = 1280, height = 720}).
#' This parameters are available only for \code{save} 'jpeg', 'png' and 'gif'.
#' For further information see \code{\link{png}} or \code{\link{jpeg}}.
#' @param res the nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer
#' (by default \code{res = 144}). This parameter is available only for \code{save} 'jpeg', 'png'.
#' For further information see \code{\link{png}} or \code{\link{jpeg}}.
#' @param delay numeric value specifying the time to show each image in seconds, when \code{save = "gif"}.
#' @param verbose logic value (T or F). If \code{TRUE} (the default) information about the function progress are printed.
#' If \code{FALSE} no message is printed.
#'
#' @details
#' \code{EEAaq_idw_map} create a spatial interpolation map, based on the Inverse Distance Weighting method (Shepard 1968).
#' This method starts from the available georeferenced data and estimates the value of the variable in the points
#' where it's unknown as a weighted average of the known values, where weights are given by an inverse function of the
#' distance of every point from the fixed stations.
#' The greater the distance of a point from a station, the smaller the weight assigned to the values of the respective
#' station for the computing of that unknown point.
#' Given the sampling plan \eqn{s_{i}} for \eqn{i=1,...,n}, which represent the location of the air quality stations,
#' the pollutant concentration value \eqn{Y(s_i)=Y_i} represents the value of the pollutant concentration detected
#' by the site \eqn{s_i} and \eqn{u} is the point for which the value of the concentration in unknown.
#' \deqn{\hat{Y}(u) = \sum_{i=1}^{n} Y_i \omega_i(u), } where
#' \deqn{\omega_i(u) = \frac{g(d(s_i,u))}{\sum_{i=1}^{n}g(d(s_i,u))} }
#' represent the weights assigned to each location \eqn{s_i} and \eqn{d(s_i,u)} is the distance between \eqn{u}
#' and \eqn{s_i}.
#'
#' @return cosa restituisce la funzione
#' @examples
#' \donttest{
#' data <- EEAaq_get_data(zone_name = "Milano", NUTS_level = "LAU",
#'   pollutant = "PM10", from = 2023, to = 2023, ID = FALSE, verbose = TRUE)
#'
#' #Monthly aggregation
#' t_aggr <- EEAaq_time_aggregate(data = data, frequency = "monthly",
#'                                 aggr_fun = c("mean", "min", "max"))
#'
#' #12 maps are generated, one for each month
#' EEAaq_idw_map(data = t_aggr, pollutant = "PM10",
#' aggr_fun = "mean", distinct = TRUE,
#' gradient = TRUE, idp = 2,
#' dynamic = FALSE)
#'
#' #Let's try to change the parameters fill_NUTS_level and dynamic
#' #Now we are going to use a dataset containing PM10 concentrations
#' #in Milan province (NUTS 3), during 2022
#' data <- EEAaq_get_data(zone_name = "Milano", NUTS_level = "NUTS3",
#'   pollutant = "PM10", from = 2022, to = 2022)
#' #yearly aggregation
#' t_aggr <- EEAaq_time_aggregate(data = data, frequency = "yearly",
#'   aggr_fun = "mean")
#' #Let's generate one dynamic map, containing the municipalities inside the Milan province
#' #filled with the mean concentration value for 2022, computed via idw:
#' EEAaq_idw_map(data = t_aggr, pollutant = "PM10", aggr_fun = "mean",
#'   distinct = TRUE, gradient = FALSE, dynamic = TRUE, fill_NUTS_level = "LAU")
#' }
#' @export

EEAaq_idw_map <- function(data = NULL, pollutant = NULL, aggr_fun, bounds_level = NULL,
                    distinct = FALSE, gradient = TRUE, idp = 2, nmax = NULL, maxdist = NULL,
                    dynamic = FALSE, fill_NUTS_level = NULL, tile = "Esri.WorldGrayCanvas",
                    save = NULL, filepath = NULL, width = 1280, height = 720,
                    res = 144, delay = 1, verbose = TRUE) {

  "%notin%" <- Negate("%in%")
  `%>%` <- dplyr::`%>%`


  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }



  #Se nei dati di input sono presenti piu inquinanti, estraggo la tabella relativa all'inquinante di interesse
  stopifnot("Only one pollutant should be specified for the parameter pollutant" = length(pollutant) == 1)
  if("EEAaq_taggr_list" %in% class(data)) {
    data <- data$TimeAggr_byPollutant[[pollutant]]
  }

  #Verifica che l'input nel parametro data sia un oggetto di classe time_aggr_AQ_df
  stopifnot("Data does not belong to class 'EEAaq_taggr_df'" = "EEAaq_taggr_df" %in% class(data) | "EEAaq_taggr_df_sfc" %in% class(data))

  #Verifica che il parametro data venga specificato
  stopifnot("The parameter data shoul be specified" = !is.null(data))

  #Se fill_NUTS_level e' specificato e l'oggetto e' di classe time_aggr_AQ_df_sfc: ERRORE
  stopifnot("An object of class EEAaq_taggr_df_sfc can not support the modality fill_NUTS_level" =  is.null(fill_NUTS_level) | "EEAaq_taggr_df_sfc" %notin% class(data) & !is.null(fill_NUTS_level))

  #Il parametro fill_NUTS_level specificato correttamente:
  if(!is.null(fill_NUTS_level)) {
    stopifnot("The parameter fill_NUTS_level should be one of NUTS0, NUTS1, NUTS2, NUTS3, LAU" = fill_NUTS_level %in% c("LAU", "NUTS0", "NUTS1", "NUTS2", "NUTS3"))
  }


  if(verbose == T) {
    cat(paste0("Map initialization started at ", Sys.time(), "\n"))
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



  #Estraggo le informazioni dall'input
  if("EEAaq_taggr_df_sfc" %notin% class(data)) {
    zone_name = attributes(data)$zone_name
    NUTS_level = attributes(data)$NUTS_level
    if(is.null(pollutant)){
      pollutant = attributes(data)$pollutants
    }
    if(length(pollutant) > 1) {
      stop("Specify the parameter pollutant.")
    }
    frequency = attributes(data)$frequency
    #Estraggo le geometrie da rappresentare
    if(NUTS_level == "LAU") {
      mappa <- LAU %>% sf::st_as_sf() %>% dplyr::filter(.data$LAU_NAME %in% zone_name)
      #mappa <- LAU[LAU$LAU_NAME %in% zone_name, ]
    } else {
      if(NUTS_level == "NUTS0" & sum(zone_name %in% unique(NUTS$CNTR_CODE)) == length(zone_name)) {
        zone_name <- dplyr::left_join(dplyr::tibble(CNTR_CODE = zone_name), dplyr::distinct(dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 0), .data$CNTR_CODE, .data$NAME_LATN), by = "CNTR_CODE") %>% dplyr::pull(.data$NAME_LATN)
        #zone_name <- dplyr::left_join(dplyr::tibble(CNTR_CODE = zone_name), dplyr::distinct(NUTS[NUTS$LEVL_CODE == 0, ], .data$CNTR_CODE, .data$NAME_LATN), by = "CNTR_CODE") %>% dplyr::pull(.data$NAME_LATN)
      }
      mappa <- NUTS %>% sf::st_as_sf() %>% dplyr::filter(.data$LEVL_CODE == code_extr(NUTS_level) & .data$NAME_LATN %in% zone_name)
      #mappa <- NUTS[NUTS$LEVL_CODE == code_extr(NUTS_level) & NUTS$NAME_LATN %in% zone_name, ]
    }
  } else if("EEAaq_taggr_df_sfc" %in% class(data)) {
    mappa <- attributes(data)$zone_geometry
    if(is.null(pollutant)){
      pollutant = attributes(data)$pollutants
    }
    if(length(pollutant) > 1) {
      stop("Specify the parameter pollutant.")
    }
    frequency = attributes(data)$frequency
    NUTS_level <- "polygon"
  }


  #Trasformo i nomi completi negli ID
  if(NUTS_level == "LAU") {
    zone_name <- LAU %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LAU_NAME %in% zone_name) %>% dplyr::pull(.data$LAU_ID)
  } else if(NUTS_level != "NUTS0" & NUTS_level != "polygon") {
    zone_name <- NUTS %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LEVL_CODE == code_extr(NUTS_level) & .data$NAME_LATN %in% zone_name) %>% dplyr::pull(.data$NUTS_ID)
  }


  #Identifico i confini interni che andranno rappresentati in base al parametro bounds_level
  if(!is.null(bounds_level) & NUTS_level != "polygon") {
    if(code_extr(NUTS_level) < code_extr(bounds_level)) {
      if(bounds_level == "LAU") {
        NUTS_LAU <- sf::st_join(dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
        #NUTS_LAU <- sf::st_join(LAU[LAU$CNTR_CODE %in% mappa$CNTR_CODE, ], NUTS[NUTS$LEVL_CODE == code_extr(NUTS_level) & NUTS$CNTR_CODE == mappa$CNTR_CODE, ], largest = T)
        bounds <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% zone_name)
        #bounds <- NUTS_LAU[NUTS_LAU$LEVL_CODE == code_extr(NUTS_level) & NUTS_LAU$NUTS_ID %in% zone_name, ]
      } else {
        bounds <- NUTS %>% sf::st_as_sf() %>% dplyr::filter(.data$LEVL_CODE == code_extr(bounds_level) & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
        #bounds <- NUTS[NUTS$LEVL_CODE == code_extr(bounds_level) & substr(NUTS$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID, ]
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
      ind <- sf::st_intersects(dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(bounds_level)), mappa, sparse = T)
      #ind <- sf::st_intersects(NUTS[NUTS$LEVL_CODE == code_extr(bounds_level), ], mappa, sparse = T)
      bounds <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(bounds_level))[as.logical(apply(as.matrix(ind), 1, sum)),]
      #bounds <- NUTS[NUTS$LEVL_CODE == code_extr(bounds_level),][as.logical(apply(as.matrix(ind), 1, sum)),]
    }
  } else {bounds <- NULL}




  #Cambio il nome della  colonna Year se la frequenza dell'oggetto time_aggr_AQ_df e' yearly
  #if(frequency == "yearly") {
  #  data <- data %>% rename(Date = Year)
  #}

  #Costruzione della griglia su cui calcolare l'idw
  num <- ifelse(as.numeric(sum(sf::st_area(mappa))) < 500000000, 0.0025 , ifelse(as.numeric(sum(sf::st_area(mappa))) < 1000000000, 0.005, 0.01))
  if("EEAaq_taggr_df_sfc" %notin% class(data)) {
    x <- seq(floor(sf::st_bbox(mappa$geometry)[1]), floor(sf::st_bbox(mappa$geometry)[3]) + 1, by=num)
    y <- seq(floor(sf::st_bbox(mappa$geometry)[2]), floor(sf::st_bbox(mappa$geometry)[4]) + 1, by=num)
  } else if("EEAaq_taggr_df_sfc" %in% class(data)) {
    x <- seq(floor(sf::st_bbox(mappa)[1]), floor(sf::st_bbox(mappa)[3]) + 1, by=num)
    y <- seq(floor(sf::st_bbox(mappa)[2]), floor(sf::st_bbox(mappa)[4]) + 1, by=num)
  }
  griglia <- expand.grid(Longitude = x, Latitude = y)
  griglia <- sf::st_as_sf(griglia, coords = c("Longitude", "Latitude"), crs = sf::st_crs(4326))

  #Aggiungo le coordinate nel dataset di input
  locations <- dplyr::left_join(data, dplyr::distinct(dplyr::filter(stations, .data$AirQualityStationEoICode %in% data$AirQualityStationEoICode), .data$AirQualityStationEoICode, .data$Longitude, .data$Latitude), by = "AirQualityStationEoICode")
  #Trasformo le coordinate in geometrie
  locations <- sf::st_as_sf(locations, coords = c("Longitude", "Latitude"), crs = 4326)
  times <- unique(data$Date)

  #Palette di colori che servira' piu' avanti
  #pal_idw <- colorNumeric(palette = c(c("green3", "greenyellow", "yellow", "orange", "red", "darkred")), domain = range(pull(stats::na.omit(locations),aggr_fun)))
  pal_idw <- switch(as.character(gradient), "FALSE" = {leaflet::colorBin(palette = c(c("green3", "greenyellow", "yellow", "orange", "red", "darkred")), domain = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), bins = 6, na.color = NA)},
                    "TRUE" = {leaflet::colorNumeric(palette = c(c("green3", "greenyellow", "yellow", "orange", "red", "darkred")), domain = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), na.color = NA)})

  if(dynamic == F & is.null(fill_NUTS_level)) {
    my_plot <- function(date) {
      if(verbose == T) {
        cat(paste0("Computing IDW interpolation for: ", date, ", ", which(date == times), " of ", length(times), "\n"))
      }
      locs <- locations %>% dplyr::filter(.data$Date == date)
      if(is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp)
      } else if(!is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax)
      } else if(is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, maxdist = maxdist)
      } else if(!is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax, maxdist = maxdist)
      }
      colnames(my_idw)[1] <- aggr_fun
      ind <- sf::st_intersects(griglia, mappa, sparse = T)
      my_idw <- my_idw[as.logical(apply(as.matrix(ind), 1, sum)),]

      raster <- data.frame(var = dplyr::pull(my_idw, aggr_fun), sf::st_coordinates(my_idw))
      colnames(raster) <- c(aggr_fun, "X", "Y")
      if(is.null(bounds)) {
        map <- ggplot2::ggplot() +
          ggplot2::geom_raster(data = raster, ggplot2::aes(x = .data$X, y = .data$Y, fill = raster[,aggr_fun])) +
          ggplot2::geom_sf(data = mappa, fill = NA, lwd = .7) +
          switch(as.character(gradient),
                 "TRUE" = {
                   ggplot2::scale_fill_gradientn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), limits = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }, "FALSE" = {
                   ggplot2::scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), n.breaks = 6, right = T, limits = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }) +
          #scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), breaks = c(0,10,20,30,40,75,100), values = scales::rescale(c(0,10,20,30,40,75,100)), limits = c(0,100), oob = scales::squish) +
          ggplot2::labs(x = "Longitude", y = "Latitude", title = paste(date)) +
          ggplot2::geom_sf(data = locations, size = .5) +
          #ggspatial::annotation_north_arrow(which_north = "true") +
          #ggspatial::annotation_scale(location="br") +
          ggplot2::theme_bw()
      } else if(!is.null(bounds)) {
        map <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = bounds, fill = NA, lwd = .2) +
          ggplot2::geom_raster(data = raster, ggplot2::aes(x = .data$X, y = .data$Y, fill = raster[,aggr_fun])) +
          ggplot2::geom_sf(data = mappa, fill = NA, lwd = .7) +
          ggplot2::geom_sf(data = bounds, fill = NA, lwd = .2) +
          switch(as.character(gradient),
                 "TRUE" = {
                   ggplot2::scale_fill_gradientn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }, "FALSE" = {
                   ggplot2::scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), n.breaks = 6, right = T, limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }) +
          #scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), breaks = c(0,10,20,30,40,75,100), values = scales::rescale(c(0,10,20,30,40,75,100)), limits = c(0,100), oob = scales::squish) +
          ggplot2::labs(x = "Longitude", y = "Latitude", title = paste(date)) +
          ggplot2::geom_sf(data = locations, size = .5) +
          #ggspatial::annotation_north_arrow(which_north = "true", height = grid::unit(.7, "cm"), width = grid::unit(.7, "cm"), style = ggspatial::north_arrow_orienteering()) +
          #ggspatial::annotation_scale(location="br") +
          ggplot2::theme_bw()
      }
      return(map)
    }
    if(verbose == T) {
      cat(paste0("Map initialization ended at ", Sys.time(), "\n"))
      cat(paste0("Computing IDW interpolation started at ", Sys.time(), "\n"))
    }
    map <- lapply(times, my_plot)
    if(verbose == T) {
      cat(paste0("Computing IDW interpolation ended at ", Sys.time(), "\n"))
    }
    if(distinct == F) {
      map <- ggpubr::ggarrange(plotlist = map)
    }



  } else if(dynamic == F & !is.null(fill_NUTS_level)) {
    my_plot <- function(date) {
      if(verbose == T) {
        cat(paste0("Computing IDW interpolation for: ", date, ", ", which(date == times), " of ", length(times), "\n"))
      }
      locs <- locations %>% dplyr::filter(.data$Date == date)
      if(is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp)
      } else if(!is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax)
      } else if(is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, maxdist = maxdist)
      } else if(!is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax, maxdist = maxdist)
      }
      colnames(my_idw)[1] <- aggr_fun
      ind <- sf::st_intersects(griglia, mappa, sparse = T)
      my_idw <- my_idw[as.logical(apply(as.matrix(ind), 1, sum)),]
      my_idw <- my_idw %>% dplyr::mutate(fill = NA, fill2 = NA)
      if(fill_NUTS_level == "LAU") {
        ind <- sf::st_intersects(my_idw$geometry, dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), sparse = T)
        #ind <- sf::st_intersects(my_idw$geometry, LAU[LAU$CNTR_CODE %in% unique(mappa$CNTR_CODE), ], sparse = T)
        suppressWarnings(my_idw[as.logical(apply(as.matrix(ind), 1, sum)), c("fill", "fill2")] <- dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE))[unlist(ind), c("LAU_NAME", "LAU_ID")])
        #suppressWarnings(my_idw[as.logical(apply(as.matrix(ind), 1, sum)), c("fill", "fill2")] <- LAU[LAU$CNTR_CODE %in% unique(mappa$CNTR_CODE), ][unlist(ind), c("LAU_NAME", "LAU_ID")])
        vals <- sf::st_drop_geometry(my_idw) %>% dplyr::group_by(.data$fill, .data$fill2) %>% dplyr::summarise(summ = mean(get(aggr_fun)), .groups = "keep") %>% dplyr::rename("LAU_NAME" = .data$fill) %>% dplyr::rename("LAU_ID" = .data$fill2)
        vals <- dplyr::left_join(vals, dplyr::select(dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), "geometry", "LAU_NAME", "LAU_ID"), by = c("LAU_NAME", "LAU_ID")) %>% sf::st_as_sf()
        #vals <- dplyr::left_join(vals, dplyr::select(LAU[LAU$CNTR_CODE %in% unique(mappa$CNTR_CODE), ], "geometry", "LAU_NAME", "LAU_ID"), by = c("LAU_NAME", "LAU_ID")) %>% sf::st_as_sf()
      } else {
        ind <- sf::st_intersects(my_idw$geometry, dplyr::filter(sf::st_as_sf(NUTS), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE) & .data$LEVL_CODE == code_extr(fill_NUTS_level)), sparse = T)
        #ind <- sf::st_intersects(my_idw$geometry, NUTS[NUTS$CNTR_CODE %in% unique(mappa$CNTR_CODE) & NUTS$LEVL_CODE == code_extr(fill_NUTS_level), ], sparse = T)
        suppressWarnings(my_idw[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- dplyr::filter(sf::st_as_sf(NUTS), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE) & .data$LEVL_CODE == code_extr(fill_NUTS_level))[unlist(ind), "NAME_LATN"])
        #suppressWarnings(my_idw[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- NUTS[NUTS$CNTR_CODE %in% unique(mappa$CNTR_CODE) & NUTS$LEVL_CODE == code_extr(fill_NUTS_level), ][unlist(ind), "NAME_LATN"])
        vals <- sf::st_drop_geometry(my_idw) %>% dplyr::group_by(.data$fill) %>% dplyr::summarise(summ = mean(get(aggr_fun))) %>% dplyr::rename("NAME_LATN" = .data$fill)
        vals <- dplyr::left_join(vals, dplyr::select(dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(fill_NUTS_level) & .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), "geometry", "NAME_LATN"), by = "NAME_LATN") %>% sf::st_as_sf()
        #vals <- dplyr::left_join(vals, dplyr::select(NUTS[NUTS$LEVL_CODE == code_extr(fill_NUTS_level) & NUTS$CNTR_CODE %in% unique(mappa$CNTR_CODE), ], "geometry", "NAME_LATN"), by = "NAME_LATN") %>% sf::st_as_sf()
      }
      #Mappa con riempimento delle zone del livello specificato senza confini interni
      if(is.null(bounds)) {
        map <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = mappa, fill = NA, lwd = .7) +
          ggplot2::geom_sf(data = vals, col = "black", ggplot2::aes(fill = .data$summ), lwd = .2) +
          switch(as.character(gradient),
                 "TRUE" = {
                   ggplot2::scale_fill_gradientn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }, "FALSE" = {
                   ggplot2::scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), n.breaks = 6, right = T, limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }) +
          #scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), breaks = c(0,10,20,30,40,75,100), values = scales::rescale(c(0,10,20,30,40,75,100)), limits = c(0,100), oob = scales::squish) +
          ggplot2::labs(x = "Longitude", y = "Latitude", title = paste(date)) +
          ggplot2::geom_sf(data = locations, size = .5) +
          #ggspatial::annotation_north_arrow(which_north = "true", height = grid::unit(.7, "cm"), width = grid::unit(.7, "cm"), style = ggspatial::north_arrow_orienteering()) +
          #ggspatial::annotation_scale(location="br") +
          ggplot2::theme_bw()
      } else if(!is.null(bounds)) {
        map <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = vals, ggplot2::aes(fill = .data$summ), lwd = .2, col = "black") +
          ggplot2::geom_sf(data = mappa, fill = NA, lwd = .8, col = "black") +
          ggplot2::geom_sf(data = bounds, fill = NA, lwd = .5) +
          switch(as.character(gradient),
                 "TRUE" = {
                   ggplot2::scale_fill_gradientn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }, "FALSE" = {
                   ggplot2::scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), n.breaks = 6, right = T, limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
                 }) +
          #scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), breaks = c(0,10,20,30,40,75,100), values = scales::rescale(c(0,10,20,30,40,75,100)), limits = c(0,100), oob = scales::squish) +
          ggplot2::labs(x = "Longitude", y = "Latitude", title = paste(date)) +
          ggplot2::geom_sf(data = locations, size = .5) +
          #ggspatial::annotation_north_arrow(which_north = "true", height = grid::unit(.7, "cm"), width = grid::unit(.7, "cm"), style = ggspatial::north_arrow_orienteering()) +
          #ggspatial::annotation_scale(location="br") +
          ggplot2::theme_bw()
      }
      return(map)
    }
    if(verbose == T) {
      cat(paste0("Map initialization ended at ", Sys.time(), "\n"))
      cat(paste0("Computing IDW interpolation started at ", Sys.time(), "\n"))
    }
    map <- lapply(times, my_plot)
    if(verbose == T) {
      cat(paste0("Computing IDW interpolation ended at ", Sys.time(), "\n"))
    }
    if(distinct == F) {
      map <- ggpubr::ggarrange(plotlist = map)
    }



    #MAPPA DINAMICA
    #NO POLIGONO
    #NO RIEMPIMENTO PER ZONE

  } else if(dynamic == T & is.null(fill_NUTS_level) & NUTS_level != "polygon") {
    map <- leaflet::leaflet(mappa) %>% leaflet::addProviderTiles(tile) %>%
      leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = F)) %>%
      leaflet::addMapPane("polygons", zIndex = 410) %>%
      leaflet::addMapPane("circles", zIndex = 420) %>%
      leaflet::addPolygons(color = "black",  weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = F),
                  options = leaflet::pathOptions(pane = "polygons"), group = ifelse(NUTS_level != "LAU", paste("NUTS", code_extr(NUTS_level)), "LAU"))

    if(NUTS_level == "NUTS0") {
      mappa_nuts1 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 1 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
      mappa_nuts2 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 2 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
      mappa_nuts3 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
      #mappa_nuts1 <- NUTS[NUTS$LEVL_CODE == 1 & substr(NUTS$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID, ]
      #mappa_nuts2 <- NUTS[NUTS$LEVL_CODE == 2 & substr(NUTS$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID, ]
      #mappa_nuts3 <- NUTS[NUTS$LEVL_CODE == 3 & substr(NUTS$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID, ]
      NUTS_LAU <- sf::st_join(dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
      #NUTS_LAU <- sf::st_join(LAU[LAU$CNTR_CODE %in% mappa$CNTR_CODE,], NUTS[NUTS$LEVL_CODE == code_extr(NUTS_level) & NUTS$CNTR_CODE == mappa$CNTR_CODE, ], largest = T)
      mappa_lau <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
      #mappa_lau <- NUTS_LAU[NUTS_LAU$LEVL_CODE == code_extr(NUTS_level) & NUTS_LAU$NUTS_ID %in% mappa$NUTS_ID, ]
      map <- leaflet::addPolygons(map = map, data = mappa_nuts1, group = "NUTS 1", color = "black",  weight = 1.5,
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
    } else if(NUTS_level == "NUTS1") {
      mappa_nuts2 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 2 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
      mappa_nuts3 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
      #mappa_nuts2 <- NUTS[NUTS$LEVL_CODE == 2 & substr(NUTS$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID, ]
      #mappa_nuts3 <- NUTS[NUTS$LEVL_CODE == 3 & substr(NUTS$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID, ]
      NUTS_LAU <- sf::st_join(dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
      #NUTS_LAU <- sf::st_join(LAU[LAU$CNTR_CODE %in% mappa$CNTR_CODE, ], NUTS[NUTS$LEVL_CODE == code_extr(NUTS_level) & NUTS$CNTR_CODE == mappa$CNTR_CODE, ], largest = T)
      mappa_lau <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
      #mappa_lau <- NUTS_LAU[NUTS_LAU$LEVL_CODE == code_extr(NUTS_level) & NUTS_LAU$NUTS_ID %in% mappa$NUTS_ID, ]
      map <- leaflet::addPolygons(map = map, data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1.2,
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
    } else if(NUTS_level == "NUTS2") {
      mappa_nuts3 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID)
      #mappa_nuts3 <- NUTS[NUTS$LEVLE_CODE == 3 & substr(NUTS$NUTS_ID,1,code_extr(NUTS_level)+2) %in% mappa$NUTS_ID]
      NUTS_LAU <- sf::st_join(dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
      #NUTS_LAU <- sf::st_join(LAU[LAU$CNTR_CODE %in% mappa$CNTR_CODE, ], NUTS[NUTS$LEVL_CODE == code_extr(NUTS_level) & NUTS$CNTR_CODE == mappa$CNTR_CODE, ], largest = T)
      mappa_lau <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
      #mappa_lau <- NUTS_LAU[NUTS_LAU$LEVL_CODE == code_extr(NUTS_level) & NUTS_LAU$NUTS_ID %in% mappa$NUTS_ID, ]
      map <- leaflet::addPolygons(map = map, data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = 1,
                         smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                         highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                         options = leaflet::pathOptions(pane = "polygons")) %>%
        leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .5,
                    smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                    highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                    options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_level == "NUTS3") {
      NUTS_LAU <- sf::st_join(dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% mappa$CNTR_CODE), dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_level) & .data$CNTR_CODE == mappa$CNTR_CODE), largest = T)
      #NUTS_LAU <- sf::st_join(LAU[LAU$CNTR_CODE %in% mappa$CNTR_CODE, ], NUTS[NUTS$LEVL_CODE == code_extr(NUTS_level) & NUTS$CNTR_CODE == mappa$CNTR_CODE, ], largest = T)
      mappa_lau <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
      #mappa_lau <- NUTS_LAU[NUTS_LAU$LEVL_CODE == code_extr(NUTS_level) & NUTS_LAU$NUTS_ID %in% mappa$NUTS_ID, ]
      map <- leaflet::addPolygons(map = map, data = mappa_lau, group = "LAU", color = "black",  weight = .5,
                         smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                         highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                         options = leaflet::pathOptions(pane = "polygons"))
    }
    if(verbose == T) {
      cat(paste0("Map initialization ended at ", Sys.time(), "\n"))
      cat(paste0("Computing IDW interpolation started at ", Sys.time(), "\n"))
    }
    for (i in 1:length(times)) {
      if(verbose == T) {
        cat(paste0("Computing IDW interpolation for: ", times[i], ", ", i, " of ", length(times), "\n"))
      }
      locs <- locations %>% dplyr::filter(.data$Date == times[i])
      if(is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp)
      } else if(!is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax)
      } else if(is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, maxdist = maxdist)
      } else if(!is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax, maxdist = maxdist)
      }

      colnames(my_idw)[1] <- aggr_fun
      ind <- sf::st_intersects(griglia, mappa, sparse = T)
      my_idw <- my_idw[as.logical(apply(as.matrix(ind), 1, sum)),]
      raster <- data.frame(mean = dplyr::pull(my_idw, aggr_fun), sf::st_coordinates(my_idw))
      raster <- raster::rasterFromXYZ(data.frame(raster[,2:3], raster[,1]))#, crs = sp::CRS("EPSG:4326"))
      raster::crs(raster) <- 4326
      #raster <- data.frame(sf::st_coordinates(my_idw), mean = dplyr::pull(my_idw, aggr_fun))
      #raster <- terra::rast(raster, type = "xyz", crs = "epsg:4326")
      map <- leaflet::addRasterImage(map = map, x = raster, colors = pal_idw, group = times[i]) %>%
        leaflet::addCircleMarkers(data = dplyr::filter(locations, .data$Date == times[i]), label = ~AirQualityStationEoICode, fillColor = "black", fillOpacity = 1, radius = 2, stroke = F, labelOptions = leaflet::labelOptions(bringToFront = T), options = leaflet::pathOptions(pane = "circles"),
                         group = times[i], popup = paste0(stringr::str_to_title(aggr_fun), ": ", round(dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), get(aggr_fun)), 2)))
    }
    if(verbose == T) {
      cat(paste0("Computing IDW interpolation ended at ", Sys.time(), "\n"))
    }


    lays <- function(x) {
      vec <- c("NUTS 0", "NUTS 1", "NUTS 2", "NUTS 3", "LAU")
      ind <- code_extr(x) + 1
      res <- vec[ind:length(vec)]
      return(res)
    }
    map <- leaflet::addLayersControl(map = map, baseGroups = times, overlayGroups = lays(NUTS_level), position = "topleft", options = leaflet::layersControlOptions(collapsed = T)) %>%
      leaflet::addLegend(pal = pal_idw, position = "topright", values = dplyr::pull(locations, get(aggr_fun)), title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit))


    #MAPPA DINAMICA
    #POLIGONO
    #NO RIEMPIMENTO PER ZONE

  } else if(dynamic == T & is.null(fill_NUTS_level) & NUTS_level == "polygon") {
    map <- leaflet::leaflet(mappa) %>% leaflet::addProviderTiles(tile) %>%
      leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = F)) %>%
      leaflet::addMapPane("polygons", zIndex = 410) %>%
      leaflet::addMapPane("circles", zIndex = 420) %>%
      leaflet::addPolygons(color = "black",  weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = F),
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
        map <- map %>% leaflet::addPolygons(data = mappa_nuts0, color = "black",  weight = 2, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
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
        map <- map %>% leaflet::addPolygons(data = mappa_nuts1, group = "NUTS 1", color = "black",  weight = 1.5,
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
        map <- map %>% leaflet::addPolygons(data = mappa_nuts2, group = "NUTS 2", color = "black",  weight = 1.2,
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
        map <- map %>% leaflet::addPolygons(data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = .5,
                                   smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                   highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                                   options = leaflet::pathOptions(pane = "polygons")) %>%
          leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                      smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                      highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                      options = leaflet::pathOptions(pane = "polygons"))
      } else if(bounds_level == "LAU") {
        ind <- sf::st_intersects(LAU, mappa, sparse = T)
        mappa_lau <- LAU[as.logical(apply(as.matrix(ind), 1, sum)),]
        map <- map %>% leaflet::addPolygons(data = mappa_lau, group = "LAU", color = "black",  weight = .2,
                                   smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0,
                                   highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_lau$LAU_NAME,
                                   options = leaflet::pathOptions(pane = "polygons"))
      }
    }
    if(verbose == T) {
      cat(paste0("Map initialization ended at ", Sys.time(), "\n"))
      cat(paste0("Computing IDW interpolation started at ", Sys.time(), "\n"))
    }
    for (i in 1:length(times)) {
      if(verbose == T) {
        cat(paste0("Computing IDW interpolation for: ", times[i], ", ", i, " of ", length(times), "\n"))
      }
      locs <- locations %>% dplyr::filter(.data$Date == times[i])
      if(is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp)
      } else if(!is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax)
      } else if(is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, maxdist = maxdist)
      } else if(!is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax, maxdist = maxdist)
      }
      colnames(my_idw)[1] <- aggr_fun
      ind <- sf::st_intersects(griglia, mappa, sparse = T)
      my_idw <- my_idw[as.logical(apply(as.matrix(ind), 1, sum)),]
      raster <- data.frame(mean = dplyr::pull(my_idw, aggr_fun), sf::st_coordinates(my_idw))
      raster <- raster::rasterFromXYZ(data.frame(raster[,2:3], raster[,1]))#, crs = sp::CRS("EPSG:4326"))
      raster::crs(raster) <- 4326
      map <- leaflet::addRasterImage(map = map, x = raster, colors = pal_idw, group = times[i]) %>%
        leaflet::addCircleMarkers(data = dplyr::filter(locations, .data$Date == times[i]), label = ~AirQualityStationEoICode, fillColor = "black", fillOpacity = 1, radius = 2, stroke = F, labelOptions = leaflet::labelOptions(bringToFront = T), options = leaflet::pathOptions(pane = "circles"),
                         group = times[i], popup = paste0(stringr::str_to_title(aggr_fun), ": ", round(dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), get(aggr_fun)), 2), "<br>",  "EoI Code: ", dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), .data$AirQualityStationEoICode), "Station name: ", dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), .data$AirQualityStationName)))
    }
    if(verbose == T) {
      cat(paste0("Computing IDW interpolation ended at ", Sys.time(), "\n"))
    }
    bounds <- attributes(mappa)$bbox

    lays <- function(x) {
      vec <- c("NUTS 0", "NUTS 1", "NUTS 2", "NUTS 3", "LAU")
      ind <- code_extr(x) + 1
      res <- vec[ind:length(vec)]
      return(res)
    }
    if(!is.null(bounds_level)) {
      map <- leaflet::addLayersControl(map = map, baseGroups = times, overlayGroups = lays(bounds_level), position = "topleft", options = leaflet::layersControlOptions(collapsed = T)) %>%
        leaflet::addLegend(pal = pal_idw, position = "topright", values = stats::na.omit(dplyr::pull(locations, get(aggr_fun))), title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit))
    } else {
      map <- leaflet::addLayersControl(map = map, baseGroups = times, position = "topleft", options = leaflet::layersControlOptions(collapsed = T)) %>%
        leaflet::addLegend(pal = pal_idw, position = "topright", values = stats::na.omit(dplyr::pull(locations, get(aggr_fun))), title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit))
    }





    #MAPPA DINAMICA
    #RIEMPIMENTO PER ZONE (NO POLIGONO)

  } else if(dynamic == T & !is.null(fill_NUTS_level)) {
    map <- leaflet::leaflet(mappa) %>% leaflet::addProviderTiles(tile) %>%
      leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = F)) %>%
      leaflet::addMapPane("raster", zIndex = 400) %>%
      leaflet::addMapPane("polygons", zIndex = 410) %>%
      leaflet::addMapPane("circles", zIndex = 420) %>%
      leaflet::addPolygons(color = "black",  weight = 1.5, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = F),
                  options = leaflet::pathOptions(pane = "polygons"))
    if(verbose == T) {
      cat(paste0("Map initialization ended at ", Sys.time(), "\n"))
      cat(paste0("Computing IDW interpolation started at ", Sys.time(), "\n"))
    }
    for (i in 1:length(times)) {
      if(verbose == T) {
        cat(paste0("Computing IDW interpolation for: ", times[i], ", ", i, " out of ", length(times), "\n"))
      }
      locs <- locations %>% dplyr::filter(.data$Date == times[i])
      if(is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp)
      } else if(!is.null(nmax) & is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax)
      } else if(is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, maxdist = maxdist)
      } else if(!is.null(nmax) & !is.null(maxdist)) {
        my_idw <- gstat::idw(formula = stats::as.formula(paste0(aggr_fun, " ~ 1")), locations = stats::na.omit(locs), newdata = griglia, idp = idp, nmax = nmax, maxdist = maxdist)
      }
      colnames(my_idw)[1] <- aggr_fun
      ind <- sf::st_intersects(griglia, mappa, sparse = T)
      my_idw <- my_idw[as.logical(apply(as.matrix(ind), 1, sum)),]
      if(fill_NUTS_level != "LAU") {
        vals <- my_idw %>% dplyr::mutate(fill = NA)
        ind <- sf::st_intersects(vals$geometry, dplyr::filter(sf::st_as_sf(NUTS), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE) & .data$LEVL_CODE == code_extr(fill_NUTS_level)), sparse = T)
        #ind <- sf::st_intersects(vals$geometry, NUTS[NUTS$CNTR_CODE %in% unique(mappa$CNTR_CODE) & NUTS$LEVL_CODE == code_extr(fill_NUTS_level), ], sparse = T)
        suppressWarnings(vals[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- dplyr::filter(sf::st_as_sf(NUTS), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE) & .data$LEVL_CODE == code_extr(fill_NUTS_level))[unlist(ind), "NAME_LATN"])
        #suppressWarnings(vals[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- NUTS[NUTS$CNTR_CODE %in% unique(mappa$CNTR_CODE) & NUTS$LEVL_CODE == code_extr(fill_NUTS_level), ][unlist(ind), "NAME_LATN"])
        vals <- sf::st_drop_geometry(vals) %>% dplyr::group_by(.data$fill) %>% dplyr::summarise(summ = mean(get(aggr_fun))) %>% dplyr::ungroup() %>% dplyr::rename("NAME_LATN" = .data$fill)
        vals <- dplyr::left_join(vals, dplyr::select(dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(fill_NUTS_level) & .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), "geometry", "NAME_LATN"), by = "NAME_LATN") %>% sf::st_as_sf(crs = 4326)
        #vals <- dplyr::left_join(vals, dplyr::select(NUTS[NUTS$LEVL_CODE == code_extr(fill_NUTS_level) & NUTS$CNTR_CODE %in% unique(mappa$CNTR_CODE), ], "geometry", "NAME_LATN"), by = "NAME_LATN") %>% sf::st_as_sf(crs = 4326)
        map <- leaflet::addPolygons(map = map, color = "black", weight = .5, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 1,
                           fillColor = pal_idw(x = vals$summ), data = vals, highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                           options = leaflet::pathOptions(pane = "polygons"), group = times[i], popup = paste0(fill_NUTS_level,": ",vals$NAME_LATN, "<br>", stringr::str_to_title(aggr_fun),": ", round(vals$summ,2))) %>%
          leaflet::addCircleMarkers(data = dplyr::filter(locations, .data$Date == times[i]), label = ~AirQualityStationEoICode, fillColor = "black", fillOpacity = 1, radius = 2, stroke = F, labelOptions = leaflet::labelOptions(bringToFront = T), options = leaflet::pathOptions(pane = "circles"),
                           group = times[i], popup = paste0(stringr::str_to_title(aggr_fun), ": ", round(dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), get(aggr_fun)), 2)))
      } else {
        vals <- my_idw %>% dplyr::mutate(fill = NA)
        ind <- sf::st_intersects(vals$geometry, dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), sparse = T)
        #ind <- sf::st_intersects(vals$geometry, LAU[LAU$CNTR_CODE %in% unique(mappa$CNTR_CODE), ], sparse = T)
        suppressWarnings(vals[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE))[unlist(ind), "LAU_NAME"])
        #suppressWarnings(vals[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- LAU[LAU$CNTR_CODE %in% unique(mappa$CNTR_CODE), ][unlist(ind), "LAU_NAME"])
        vals <- sf::st_drop_geometry(vals) %>% dplyr::group_by(.data$fill) %>% dplyr::summarise(summ = mean(get(aggr_fun))) %>% dplyr::ungroup() %>% dplyr::rename("LAU_NAME" = .data$fill)
        vals <- dplyr::left_join(vals, dplyr::select(dplyr::filter(sf::st_as_sf(LAU), .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), "geometry", "LAU_NAME"), by = "LAU_NAME") %>% sf::st_as_sf(crs = 4326)
        #vals <- dplyr::left_join(vals, dplyr::select(LAU[LAU$CNTR_CODE %in% unique(mappa$CNTR_CODE), ], "geometry", "LAU_NAME"), by = "LAU_NAME") %>% sf::st_as_sf(crs = 4326)
        map <- leaflet::addPolygons(map = map, color = "black", weight = .5, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 1,
                           fillColor = pal_idw(x = vals$summ), data = vals, highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                           options = leaflet::pathOptions(pane = "polygons"), group = times[i], popup = paste0(fill_NUTS_level,": ",vals$LAU_NAME, "<br>", stringr::str_to_title(aggr_fun),": ", round(vals$summ,2))) %>%
          leaflet::addCircleMarkers(data = dplyr::filter(locations, .data$Date == times[i]), label = ~AirQualityStationEoICode, fillColor = "black", fillOpacity = 1, radius = 2, stroke = F, labelOptions = leaflet::labelOptions(bringToFront = T), options = leaflet::pathOptions(pane = "circles"),
                           group = times[i], popup = paste0(stringr::str_to_title(aggr_fun), ": ", round(dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), get(aggr_fun)), 2)))
      }
    }
    if(verbose == T) {
      cat(paste0("Computing IDW interpolation ended at ", Sys.time(), "\n"))
    }
    map <- leaflet::addLayersControl(position = "topleft", map = map, baseGroups = times, options = leaflet::layersControlOptions(collapsed = T)) %>%
      leaflet::addLegend(position = "topright", pal = pal_idw, values = dplyr::pull(locations, get(aggr_fun)), title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit), opacity = 1)
  }


  #SALVATAGGIO

  if(verbose == T & !is.null(save)) {
    cat(paste0("Plot saving started at ", Sys.time(), "\n"))
  }
  if(!is.null(save)){
    if(distinct == F & save == "gif") {
      stop("Can not save the plots as a gif if distinct = F")
    }
    "%notin%" <- Negate("%in%")

    if(save %notin% c("gif", "jpeg", "png", "pdf", "html")) {
      stop("save should be either gif, png, jpeg or pdf")
    }

    if("gif" %in% save) {
      #stopifnot("The package \'gifski\' is required for saving the plot as a gif. " = "gifski" %in% rownames(utils::installed.packages()))
      stopifnot("Can not save as GIF" = "ggarrange" %notin% class(map))
      gifski::save_gif(print(map), gif_file = filepath, width = width, height = height, res = res, delay = delay)
    } else if(save == "pdf") {
      do.call(save, args = list(file = filepath))
      print(map)
      grDevices::dev.off()
    } else if(save %in% c("jpeg", "png")) {
      ind <- 1:length(map)
      filepath <- paste0(substr(filepath, 1, ifelse(save == "jpeg", nchar(filepath) - 5, nchar(filepath) - 4)), ind, ifelse(save == "jpeg", ".jpeg", ".png"))
      for (i in 1:length(map)) {
        do.call(save, args = list(file = filepath[i], width = width, height = height, res = res))
        print(map[i])
        grDevices::dev.off()
      }
    } else if(save %in% "html") {
      stopifnot("Can not save as html if dynamic = F" = dynamic == T)
      htmlwidgets::saveWidget(map, file = filepath, selfcontained = F)
    } else {
      stop("Can not save the plots in this format, type should be either 'pdf', 'jpeg' or 'png'.")
    }
  }
  if(verbose == T & !is.null(save)) {
    cat(paste0("Plot saving ended at ", Sys.time(), "\n"))
  }

  return(map)

}





