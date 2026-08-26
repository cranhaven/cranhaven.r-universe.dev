#' Build a spatial interpolation map based on the Inverse Distance Weighting technique.
#' The function \code{EEAaq_idw_map} requires as input a \code{EEAaq_taggr_df} or a \code{EEAaq_taggr_df_sfc} class object and produces a
#' spatial interpolation map. Depending on the time frequency of the aggregation, multiple maps are generated, one for
#' each timestamp. Interpolation maps may be exported as pdf, jpeg, png, gif and html.
#'
#' @param data an object of class \code{EEAaq_taggr_df} or \code{EEAaq_taggr_df_sfc}, which is the
#' output of the \code{\link{EEAaq_time_aggregate}} function.
#' @param pollutant vector containing the pollutant for which to build the map. It must be one of the pollutants
#' contained in \code{data}.
#' @param aggr_fun character containing the aggregation function to use for computing the interpolation. It must
#' be one of the statistics contained in \code{data}.
#' @param NUTS_filler character containing the NUTS level or LAU for which to aggregate the idw computing,
#' in order to obtain a uniform coloring inside each area at the specified level.
#' Recall that the NUTS classification (Nomenclature of territorial units for statistics) is a hierarchical system for dividing up the economic territory of the EU and the UK.
#' The levels are defined as follows:
#' \itemize{
#' \item{\strong{NUTS 0}: the whole country}
#' \item{\strong{NUTS 1}: major socio-economic regions}
#' \item{\strong{NUTS 2}: basic regions for the application of regional policies}
#' \item{\strong{NUTS 3}: small regions for specific diagnoses}
#' \item{\strong{LAU}: municipality}
#' }
#' For instance if \code{NUTS_filler = "LAU"}, each municipality is filled by the mean value of the pollutant
#' concentration, computed by the idw, of each pixel inside the respective municipality). Allowed values are
#' 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', and 'LAU'.
#' @param NUTS_extborder character containing the NUTS level or LAU for which draw external boundaries.
#' Admissible values are 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' @param NUTS_intborder character containing the NUTS level or LAU for which draw internal boundaries.
#' Admissible values are 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' @param idp numeric value that specify the inverse distance weighting power. For further information see
#' \code{\link[gstat]{idw}}.
#' @param nmax numeric value; specify the number of nearest observations that should be
#' used for the inverse distance weighting computing, where nearest is defined in terms of the
#' space of the spatial locations. By default, all observations are used. For further information see
#' \code{\link[gstat]{idw}}
#' @param maxdist numeric value; only observations within a distance of \code{maxdist} from the prediction location
#' are used for the idw computation. By default, all observations are used.
#' If combined with \code{nmax}, both criteria apply.
#' @param dynamic logic value (T or F). If \code{TRUE} the function creates a Leaflet map widget using
#' \bold{htmlwidgets} (for further information see \code{\link[leaflet]{leaflet}}). If \code{FALSE} (the default)
#' the maps generated are static.
#' @param distinct logic value (T or F). If \code{TRUE}, each map generated is printed and saved in distinct pages
#' (for instance if \code{data} has a monthly frequency in a yearly time window, 12 distinct plots are generated).
#' If \code{FALSE} (the default), the maps are printed in a single page.
#' @param gradient logic value (T or F). If \code{TRUE} (the default) the maps generated are colored with a
#' continuous color scale. If \code{FALSE}, the color scale is discrete.

#' @param tile character representing the name of the provider tile. To see the full list of the providers, run
#' \code{\link[leaflet]{providers}}. For further information see \code{\link[leaflet]{addProviderTiles}}.
#' @param filepath a character string giving the file path.
#' @param width,height the width and the height of the plot, expressed in pixels (by default \code{width = 1280, height = 720}).
#' This parameters are available only for \code{save} 'jpeg', 'png' and 'gif'.
#' For further information see \code{\link{png}} or \code{\link{jpeg}}.
#' @param res the nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer
#' (by default \code{res = 144}). This parameter is available only for \code{save} 'jpeg', 'png'.
#' For further information see \code{\link{png}} or \code{\link{jpeg}}.
#' @param delay numeric value specifying the time to show each image in seconds, when \code{save = "gif"}.
#' @param save character representing in which extension to save the map. Allowed values are 'jpeg', 'png', 'pdf'
#' (if \code{dynamic = FALSE}), 'gif' (if \code{dynamic = FALSE & distinct = TRUE}), 'html' (if \code{dynamic = TRUE}).
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
#' \dontrun{
#' ### Filter all the stations installed in the city (LAU) of Milano (Italy)
#' IDstations <- EEAaq_get_stations(byStation = FALSE, complete = FALSE)
#'  `%>%` <- dplyr::`%>%`
#' IDstations <- IDstations %>%
#'                 dplyr::filter(LAU_NAME == "Milano") %>%
#'                 dplyr::pull(AirQualityStationEoICode) %>%
#'                 unique()
#' ### Download NO2 measurement for the city of Milano from January 1st to December 31st, 2023
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "NO2",
#'                        from = "2023-01-01", to = "2023-01-31", verbose = TRUE)
#'
#' ### Monthly aggregation: compute station-specific monthly minimum,
#' ## average, and maximum NO2 concentrations
#' t_aggr <- EEAaq_time_aggregate(data = data, frequency = "monthly",
#'                                aggr_fun = c("mean", "min", "max"))
#'
#' ### Static IDW interpolation of the average NO2 concentrations for the whole Lombardy
#' ###    region (NUTS_extborder = "NUTS2"). Interpolated values are then aggregated at the provincial
#' ###    level (NUTS_filler = "NUTS3")
#' EEAaq_idw_map(data = t_aggr, pollutant = "NO2", aggr_fun = "mean",
#'               distinct = TRUE, gradient = FALSE,
#'               dynamic = FALSE,
#'               NUTS_filler = "NUTS3", NUTS_extborder = "NUTS2")
#'
#' ### Dynamic IDW interpolation map (interactive leafleat) of the average
#' ## NO2 concentrations for the whole Lombardy
#' ###    region (NUTS_extborder = "NUTS2"). Interpolated values are then aggregated at the municipal
#' ###    level (NUTS_filler = "LAU")
#' EEAaq_idw_map(data = t_aggr, pollutant = "NO2", aggr_fun = "mean",
#'               distinct = TRUE, gradient = FALSE,
#'               dynamic = TRUE,
#'               NUTS_filler = "LAU", NUTS_extborder = "NUTS2", NUTS_intborder = "LAU")
#' }
#'
#' @export

EEAaq_idw_map <- function(data = NULL, pollutant = NULL, aggr_fun, distinct = FALSE,
                          gradient = TRUE, idp = 2, nmax = NULL, maxdist = NULL,
                          NUTS_filler = NULL, NUTS_extborder = NULL, NUTS_intborder = NULL,
                          dynamic = FALSE, tile = "Esri.WorldGrayCanvas",
                          filepath = NULL, width = 1280, height = 720,
                          res = 144, delay = 1, save = NULL, verbose = TRUE) {

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

  # Se nei dati di input sono presenti piu inquinanti, estraggo la tabella relativa all'inquinante di interesse
  stopifnot("Only one pollutant should be specified for the parameter pollutant" = length(pollutant) == 1)
  if("EEAaq_taggr_list" %in% class(data)) {
    data <- data$TimeAggr_byPollutant[[pollutant]]
  }

  # Verifica che l'input nel parametro data sia un oggetto di classe time_aggr_AQ_df
  stopifnot("Data does not belong to class 'EEAaq_taggr_df'" = "EEAaq_taggr_df" %in% class(data) | "EEAaq_taggr_df_sfc" %in% class(data))

  #Verifica che il parametro data venga specificato
  stopifnot("The parameter data shoul be specified" = !is.null(data))

  #Se NUTS_filler e' specificato e l'oggetto e' di classe time_aggr_AQ_df_sfc: ERRORE
  stopifnot("An object of class EEAaq_taggr_df_sfc can not support the modality NUTS_filler" =  is.null(NUTS_filler) | "EEAaq_taggr_df_sfc" %notin% class(data) & !is.null(NUTS_filler))

  # Check on "NUTS_"
  if(!is.null(NUTS_filler)) {
    stopifnot("The parameter NUTS_filler should be one of NUTS0, NUTS1, NUTS2, NUTS3, LAU" = NUTS_filler %in% c("LAU", "NUTS0", "NUTS1", "NUTS2", "NUTS3"))
  }
  if(!is.null(NUTS_extborder)) {
    stopifnot("The parameter NUTS_extborder should be one of NUTS0, NUTS1, NUTS2, NUTS3, LAU" = NUTS_extborder %in% c("LAU", "NUTS0", "NUTS1", "NUTS2", "NUTS3"))
  }
  if(!is.null(NUTS_intborder)) {
    stopifnot("The parameter NUTS_intborder should be one of NUTS0, NUTS1, NUTS2, NUTS3, LAU" = NUTS_intborder %in% c("LAU", "NUTS0", "NUTS1", "NUTS2", "NUTS3"))
  }

  if(verbose == T) {
    cat(paste0("Map initialization started at ", Sys.time(), "\n"))
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
                                         code_extr(NUTS_intborder),
                                         code_extr(NUTS_filler)),
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


  #############################
  ##### Building IDW grid #####
  #############################
  num <- ifelse(as.numeric(sum(sf::st_area(mappa))) < 500000000,
                0.0025,
                ifelse(as.numeric(sum(sf::st_area(mappa))) < 1000000000, 0.005, 0.01))
  if("EEAaq_taggr_df_sfc" %notin% class(data)) {
    x <- seq(floor(sf::st_bbox(mappa$geometry)[1]), floor(sf::st_bbox(mappa$geometry)[3]) + 1, by=num)
    y <- seq(floor(sf::st_bbox(mappa$geometry)[2]), floor(sf::st_bbox(mappa$geometry)[4]) + 1, by=num)
  } else if("EEAaq_taggr_df_sfc" %in% class(data)) {
    x <- seq(floor(sf::st_bbox(mappa)[1]), floor(sf::st_bbox(mappa)[3]) + 1, by=num)
    y <- seq(floor(sf::st_bbox(mappa)[2]), floor(sf::st_bbox(mappa)[4]) + 1, by=num)
  }
  griglia <- expand.grid(Longitude = x, Latitude = y)
  griglia <- sf::st_as_sf(griglia, coords = c("Longitude", "Latitude"), crs = sf::st_crs(4326))

  ##### Aggiungo le coordinate nel dataset di input
  locations <- dplyr::left_join(
    x = data,
    y = dplyr::distinct(filter_stations, .data$AirQualityStationEoICode, .data$Longitude, .data$Latitude),
    by = "AirQualityStationEoICode"
  )
  locations <- locations %>%
    dplyr::filter(!is.na(.data$AirQualityStationEoICode))
  locations <- sf::st_as_sf(locations, coords = c("Longitude", "Latitude"), crs = 4326)
  times <- unique(data$Date)

  ##### Colors palette
  pal_idw <- switch(
    as.character(gradient),
    "FALSE" = {
      leaflet::colorBin(palette = c(c("green3", "greenyellow", "yellow", "orange", "red", "darkred")),
                        domain = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), bins = 6, na.color = NA)
    },
    "TRUE" = {
      leaflet::colorNumeric(palette = c(c("green3", "greenyellow", "yellow", "orange", "red", "darkred")),
                            domain = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), na.color = NA)
    }
  )


  ###################
  ##### Mapping #####
  ###################

  if(dynamic == F & is.null(NUTS_filler)) {
    ################################################################
    ##### Case n.1: Static map without areal filling (gridded) #####
    ################################################################
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

      if(NUTS_intborder == "NUTS0") {
        mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 0)
      } else if(NUTS_intborder == "NUTS1") {
        mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 1)
      } else if(NUTS_intborder == "NUTS2") {
        mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 2)
      } else if(NUTS_intborder == "NUTS3") {
        mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 3)
      } else if(NUTS_intborder == "LAU") {
        # NUTS_LAU <- sf::st_join(x = sf::st_as_sf(LAU),
        #                         dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_extborder)),
        #                         largest = T)
        # mappa_internal <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_intborder) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
        mappa_internal <- LAU
      }

      map <- ggplot2::ggplot() +
        ggplot2::geom_raster(data = raster, ggplot2::aes(x = .data$X, y = .data$Y, fill = raster[,aggr_fun])) +
        ggplot2::geom_sf(data = mappa, fill = NA, lwd = .7) +
        ggplot2::geom_sf(data = mappa_internal, fill = NA, lwd = .7) +
        switch(as.character(gradient),
               "TRUE" = {
                 ggplot2::scale_fill_gradientn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), limits = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
               }, "FALSE" = {
                 ggplot2::scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), n.breaks = 6, right = T, limits = range(dplyr::pull(stats::na.omit(locations),aggr_fun)), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
               }) +
        ggplot2::labs(x = "Longitude", y = "Latitude", title = paste(date)) +
        ggplot2::geom_sf(data = locations, size = .5) +
        ggspatial::annotation_north_arrow(which_north = "true") +
        ggspatial::annotation_scale(location="br") +
        ggplot2::theme_bw()
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
  } else if(dynamic == F & !is.null(NUTS_filler)) {


    ##########################################################################
    ##### Case n.2: Static map with user-defined areal filling (gridded) #####
    ##########################################################################
    my_plot <- function(date) {
      if(verbose == T) {
        cat(paste0("Computing IDW interpolation for: ", date, ", ", which(date == times), " of ", length(times), "\n"))
      }
      locs <- locations %>%
        dplyr::filter(.data$Date == date)
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
      my_idw <- my_idw %>%
        dplyr::mutate(fill = NA, fill2 = NA)
      if(NUTS_filler == "LAU") {
        ind <- sf::st_intersects(x = my_idw$geometry,
                                 y = dplyr::filter(sf::st_as_sf(LAU), .data$ISO %in% unique(mappa$CNTR_CODE)),
                                 sparse = T)
        suppressWarnings(
          my_idw[as.logical(apply(as.matrix(ind), 1, sum)), c("fill", "fill2")] <- dplyr::filter(sf::st_as_sf(LAU), .data$ISO %in% unique(mappa$CNTR_CODE))[unlist(ind), c("LAU_NAME", "LAU_ID")]
        )
        vals <- sf::st_drop_geometry(my_idw) %>%
          dplyr::group_by(.data$fill, .data$fill2) %>%
          dplyr::summarise(summ = mean(get(aggr_fun)), .groups = "keep") %>%
          dplyr::rename("LAU_NAME" = .data$fill) %>%
          dplyr::rename("LAU_ID" = .data$fill2)
        vals <- dplyr::left_join(x = vals,
                                 y = dplyr::select(dplyr::filter(sf::st_as_sf(LAU), .data$ISO %in% unique(mappa$CNTR_CODE)), "geometry", "LAU_NAME", "LAU_ID"),
                                 by = c("LAU_NAME", "LAU_ID")) %>%
          sf::st_as_sf()
      } else {
        ind <- sf::st_intersects(x = my_idw$geometry,
                                 y = dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_filler)),
                                 sparse = T)
        suppressWarnings(
          my_idw[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- sf::st_as_sf(NUTS)[unlist(ind), "NAME_LATN"]
        )
        vals <- sf::st_drop_geometry(my_idw) %>%
          dplyr::group_by(.data$fill) %>%
          dplyr::summarise(summ = mean(get(aggr_fun))) %>%
          dplyr::rename("NAME_LATN" = .data$fill)
        vals <- dplyr::left_join(x = vals,
                                 y = dplyr::select(dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_filler) & .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), "geometry", "NAME_LATN"),
                                 by = "NAME_LATN") %>%
          sf::st_as_sf()
      }
      map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = mappa, fill = NA, lwd = .7) +
        ggplot2::geom_sf(data = vals, col = "black", ggplot2::aes(fill = .data$summ), lwd = .2) +
        switch(as.character(gradient),
               "TRUE" = {
                 ggplot2::scale_fill_gradientn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
               }, "FALSE" = {
                 ggplot2::scale_fill_stepsn(colours = c("green3", "greenyellow", "yellow", "orange", "red", "darkred"), n.breaks = 6, right = T, limits = c(0,max(dplyr::pull(stats::na.omit(locations),aggr_fun))), guide = ggplot2::guide_colorbar(title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)))
               }) +
        ggplot2::labs(x = "Longitude", y = "Latitude", title = paste(date)) +
        ggplot2::geom_sf(data = locations, size = .5) +
        ggspatial::annotation_north_arrow(which_north = "true", height = grid::unit(.7, "cm"), width = grid::unit(.7, "cm"), style = ggspatial::north_arrow_orienteering()) +
        ggspatial::annotation_scale(location="br") +
        ggplot2::theme_bw()
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
  } else if(dynamic == T & is.null(NUTS_filler)) {


    #################################################################
    ##### Case n.3: Dynamic map without areal filling (gridded) #####
    #################################################################
    map <- leaflet::leaflet(mappa) %>%
      leaflet::addProviderTiles(tile) %>%
      leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = F)) %>%
      leaflet::addMapPane("polygons", zIndex = 410) %>%
      leaflet::addMapPane("circles", zIndex = 420) %>%
      leaflet::addPolygons(color = "black",  weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                           highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = F),
                           options = leaflet::pathOptions(pane = "polygons"),
                           group = ifelse(NUTS_filler != "LAU", paste("NUTS", code_extr(NUTS_filler)), "LAU"))

    if(NUTS_intborder == "NUTS0") {
      mappa_nuts0 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 0 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      map <- leaflet::addPolygons(map = map, data = mappa_nuts0, group = "NUTS 0", color = "black",  weight = 1,
                                  smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                                  options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_intborder == "NUTS1") {
      mappa_nuts1 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 1 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      map <- leaflet::addPolygons(map = map, data = mappa_nuts1, group = "NUTS 1", color = "black",  weight = 1,
                                  smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                                  options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_intborder == "NUTS2") {
      mappa_nuts2 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 2 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      map <- leaflet::addPolygons(map = map, data = mappa_nuts3, group = "NUTS 2", color = "black",  weight = 1,
                                  smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                                  options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_intborder == "NUTS3") {
      mappa_nuts3 <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == 3 & substr(.data$NUTS_ID,1,code_extr(NUTS_extborder)+2) %in% mappa$NUTS_ID)
      map <- leaflet::addPolygons(map = map, data = mappa_nuts3, group = "NUTS 3", color = "black",  weight = 1,
                                  smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0,
                                  highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T), popup = mappa_nuts3$NAME_LATN,
                                  options = leaflet::pathOptions(pane = "polygons"))
    } else if(NUTS_intborder == "LAU") {
      NUTS_LAU <- sf::st_join(x = sf::st_as_sf(LAU),
                              y = dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_extborder)),
                              largest = T)
      mappa_lau <- dplyr::filter(sf::st_as_sf(NUTS_LAU), .data$LEVL_CODE == code_extr(NUTS_extborder) & .data$NUTS_ID %in% dplyr::pull(mappa, .data$NUTS_ID))
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
      locs <- locations %>%
        dplyr::filter(.data$Date == times[i])
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
        ##### Add stations coordinates
        leaflet::addCircleMarkers(
          data = dplyr::filter(locations, .data$Date == times[i]),
          label = ~.data$AirQualityStationEoICode,
          fillColor = "black", fillOpacity = 1, radius = 2, stroke = F,
          labelOptions = leaflet::labelOptions(bringToFront = T),
          options = leaflet::pathOptions(pane = "circles"),
          group = times[i],
          popup = paste("Air Quality Station EoI Code:", locations$AirQualityStationEoICode,
                        "<br>","Air Quality Station Name:", locations$AirQualityStationName,
                        "<br>","Pollutants:", pollutant,
                        "<br>",paste0(aggr_fun, ": ", round(dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), get(aggr_fun)), 2)))
        )
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

    map <- leaflet::addLayersControl(map = map, baseGroups = times, position = "topleft", options = leaflet::layersControlOptions(collapsed = T)) %>%
      leaflet::addLegend(
        pal = pal_idw, position = "topright",
        values = dplyr::pull(locations, get(aggr_fun)),
        title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit)
      )
  } else if(dynamic == T & !is.null(NUTS_filler)) {

    ###########################################################################
    ##### Case n.4: Dynamic map with user-defined areal filling (gridded) #####
    ###########################################################################
    map <- leaflet::leaflet(mappa) %>%
      leaflet::addProviderTiles(tile) %>%
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
      locs <- locations %>%
        dplyr::filter(.data$Date == times[i])
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
      if(NUTS_filler != "LAU") {
        vals <- my_idw %>%
          dplyr::mutate(fill = NA)
        ind <- sf::st_intersects(x = vals$geometry,
                                 y = dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_filler)),
                                 sparse = T)
        suppressWarnings(
          vals[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_filler))[unlist(ind), "NAME_LATN"]
        )
        vals <- sf::st_drop_geometry(vals) %>%
          dplyr::group_by(.data$fill) %>%
          dplyr::summarise(summ = mean(get(aggr_fun))) %>%
          dplyr::ungroup() %>%
          dplyr::rename("NAME_LATN" = .data$fill)
        vals <- dplyr::left_join(x = vals,
                                 y = dplyr::select(dplyr::filter(sf::st_as_sf(NUTS), .data$LEVL_CODE == code_extr(NUTS_filler) & .data$CNTR_CODE %in% unique(mappa$CNTR_CODE)), "geometry", "NAME_LATN"),
                                 by = "NAME_LATN") %>%
          sf::st_as_sf(crs = 4326)
        map <- leaflet::addPolygons(map = map, color = "black", weight = .5, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 1,
                                    fillColor = pal_idw(x = vals$summ), data = vals, highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
                                    options = leaflet::pathOptions(pane = "polygons"), group = times[i], popup = paste0(NUTS_filler,": ",vals$NAME_LATN, "<br>", aggr_fun,": ", round(vals$summ,2))) %>%
          leaflet::addCircleMarkers(data = dplyr::filter(locations, .data$Date == times[i]), label = ~.data$AirQualityStationEoICode, fillColor = "black", fillOpacity = 1, radius = 2, stroke = F, labelOptions = leaflet::labelOptions(bringToFront = T), options = leaflet::pathOptions(pane = "circles"),
                                    group = times[i], popup = paste0(aggr_fun, ": ", round(dplyr::pull(dplyr::filter(locations, .data$Date == times[i]), get(aggr_fun)), 2)))
      } else {
        vals <- my_idw %>%
          dplyr::mutate(fill = NA)
        ind <- sf::st_intersects(x = vals$geometry,
                                 y = dplyr::filter(sf::st_as_sf(LAU), .data$ISO %in% unique(mappa$CNTR_CODE)),
                                 sparse = T)
        suppressWarnings(
          vals[as.logical(apply(as.matrix(ind), 1, sum)), "fill"] <- dplyr::filter(sf::st_as_sf(LAU), .data$ISO %in% unique(mappa$CNTR_CODE))[unlist(ind), "LAU_NAME"]
        )
        vals <- sf::st_drop_geometry(vals) %>%
          dplyr::group_by(.data$fill) %>%
          dplyr::summarise(summ = mean(get(aggr_fun))) %>%
          dplyr::ungroup() %>% dplyr::rename("LAU_NAME" = .data$fill)
        vals <- dplyr::left_join(x = vals,
                                 y = dplyr::select(dplyr::filter(sf::st_as_sf(LAU), .data$ISO %in% unique(mappa$CNTR_CODE)), "geometry", "LAU_NAME"),
                                 by = "LAU_NAME") %>%
          sf::st_as_sf(crs = 4326)
        map <- leaflet::addPolygons(
          map = map, color = "black", weight = .5, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 1,
          fillColor = pal_idw(x = vals$summ),
          data = vals, highlightOptions = leaflet::highlightOptions(color = "white", weight = 2, bringToFront = T),
          options = leaflet::pathOptions(pane = "polygons"),
          group = times[i],
          popup = paste0(NUTS_filler,": ",vals$LAU_NAME,
                         "<br>","Date:", times[i],
                         "<br>","Pollutant:", pollutant,
                         "<br>", aggr_fun,": ", round(vals$summ,2))
        )
      }
    }
    if(verbose == T) {
      cat(paste0("Computing IDW interpolation ended at ", Sys.time(), "\n"))
    }
    map <- leaflet::addLayersControl(position = "topleft", map = map, baseGroups = times, options = leaflet::layersControlOptions(collapsed = T)) %>%
      leaflet::addLegend(position = "topright", pal = pal_idw, values = dplyr::pull(locations, get(aggr_fun)), title = dplyr::pull(dplyr::filter(pollutants, .data$Notation == pollutant), .data$RecommendedUnit), opacity = 1)
  }


  #########################
  ##### Export figure #####
  #########################
  if(!is.null(save)){
    if(verbose == T) {
      cat(paste0("Plot saving started at ", Sys.time(), "\n"))
    }

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
    if(verbose == T) {
      cat(paste0("Plot saving ended at ", Sys.time(), "\n"))
    }
  }

  ##################
  ##### Output #####
  ##################
  return(map)

}
