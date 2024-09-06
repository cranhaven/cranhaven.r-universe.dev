#' Download air quality data at european level from the EEA download service
#'
#' This function imports air quality datasets at european level, based on the zone, time and pollutant specifications.
#' This function generates an \code{EEAaq_df} object, or an \code{EEAaq_df_sfc}.
#' @param zone_name character vector specifying the names of the zones to consider. The reference is the NUTS and LAU nomenclature by Eurostat.
#' See \emph{Details}.
#' @param NUTS_level character that specify the level of NUTS or LAU, to which \code{zone_name} belongs.
#' Allowed values are 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' For further information see \emph{Details}.
#' @param pollutant the pollutants for which to download data. It may be:
#' \itemize{
#' \item{character vector representing the short names of the pollutants to analyse. The reference is the
#' variable \code{Notation} in the dataset \code{pollutants} provided by this package.}
#' \item{numeric vector representing the codes of the pollutants to analyse. The reference is the variable \code{Code}
#' in the dataset \code{pollutants} provided by this package.}
#' }
#' @param from the starting point of the time window to consider.
#' It may be:
#' \itemize{
#' \item{integer representing the year (data are downloaded starting from the first day of the year specified)}
#' \item{character containing a specific day of the year in the format \code{yyyy-mm-dd}}
#' }
#' @param to the ending point of the time window to consider.
#' It may be:
#' \itemize{
#' \item{integer representing the year (data are downloaded ending at the last day of the year specified)}
#' \item{character containing a specific day of the year in the format \code{yyyy-mm-dd}}
#' }
#' @param ID logic value (T or F). If \code{TRUE}, the character specified in the parameter \code{zone_name}
#'  is the unique identifier code provided by Eurostat. The reference is the \code{NUTS_ID} column from the
#'  \code{NUTS} dataset or the \code{LAU_ID} from the \code{LAU} dataset.
#'  If \code{FALSE} (the default), the character specified in the parameter \code{zone_name} is the zone name
#'  expressed in latin characters. The reference is the \code{NAME_LATN} column from the \code{NUTS} dataset and the
#'  \code{LAU_NAME} column from the \code{LAU} dataset.
#' @param quadrant a list of bidimensional numeric vectors containing the coordinates in \bold{WGS84} format.
#'                  If the list has two elements, the function builds a square using the two coordinates as
#'                  opposite extremes. If the list contains three or more elements, every point is a vertex of a
#'                  polygon, in particular the convex hull of the specified points.
#' @param polygon A \code{sfc_POLYGON} or \code{sfc_MULTIPOLYGON} class object
#' (see <https://CRAN.R-project.org/package=sf> for further information), specifying the area of interest.
#' The polygon can be imported via shapefile or other formats from the user.
#' @param verbose logic value (T or F). If \code{TRUE} (the default) information about
#' the function progress are printed. If \code{FALSE} no message is printed.
#'
#' @details
#' The NUTS classification (Nomenclature of territorial units for statistics) is a hierarchical system for dividing up the economic territory of the EU and the UK.
#' The levels are defined as follows:
#' \itemize{
#' \item{\strong{NUTS 0}: the whole country}
#' \item{\strong{NUTS 1}: major socio-economic regions}
#' \item{\strong{NUTS 2}: basic regions for the application of regional policies}
#' \item{\strong{NUTS 3}: small regions for specific diagnoses}
#' }
#' Further information is available at <https://ec.europa.eu/eurostat/web/nuts/background>.
#' These LAUs (Local Administrative Units) are the building blocks of the NUTS, and comprise the municipalities and communes of the European Union.
#' For further information see <https://ec.europa.eu/eurostat/web/nuts/local-administrative-units>.
#' Note that a specific name can be associated with both LAU and NUTS levels.
#' For instance "Milano" is either a city or a NUTS 3 area in Italy. To download data referred to the city,
#' specify \code{zone_name = "Milano"} and \code{NUTS_level = "LAU"}, while to download data referred to the province, specify
#' \code{zone_name = "Milano"} and \code{NUTS_level = "NUTS3"}.
#' One of \code{zone_name}, \code{quadrant} and \code{polygon} should always be specified by the user.
#'
#'
#' @return A data frame of class \code{EEAaq_df}, if \code{zone_name} is specified, and of class \code{EEAaq_df_sfc}
#' if whether the parameter \code{quadrant} or \code{polygon} is specified.
#' @examples
#'
#' \donttest{
#' library(dplyr)
#' library(utils)
#' #Download of the PM10 data in Milan (Lombardia, Italy) during 2023.
#' EEAaq_get_data(zone_name = "Milano", NUTS_level = "LAU",
#'   pollutant = "PM10", from = 2023, to = 2023, ID = FALSE, verbose = TRUE)
#'
#' #Alternatively, it is possible to obtain the same result using
#' #the LAU_ID of Milan and the pollutant code:
#' EEAaq_get_data(zone_name = "015146", NUTS_level = "LAU",
#'   pollutant = "PM10", from = 2023, to = 2023, ID = TRUE, verbose = TRUE)
#'
#' #Another way to get the nitrogen dioxide in Milan during 2023,
#' #is to extract the geometry from the LAU dataset and use it with the polygon parameter:
#' temp <- tempfile()
#' download.file("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/LAU.rds",
#'   temp, quiet = TRUE)
#' milan <- readRDS(temp) %>%
#'     filter(LAU_NAME == "Milano") %>%
#'     pull(geometry)
#' EEAaq_get_data(pollutant = "PM10", from = 2023, to = 2023, polygon = milan)
#'
#' #Another option is to choose the exact dates, for example,
#' #in order to import Milan PM10 january 2023 data,
#' #it is possible to write:
#' EEAaq_get_data(polygon = milan, pollutant = "PM10", from = "2023-01-01", to = "2023-01-31")
#'
#' #Spcifying the parameter quadrant, it is possible to choose
#' #a specific quadrant on the geographic map.
#' #For instance, using the points (11.5, 46.5), (10.5, 46),
#' #will be imported data about the air quality stations
#' #located inside the rectangle which has, as opposite vertexes,
#' #the two selected points. In this case
#' #PM10 and PM2.5 data are downloaded for the whole 2021:
#' EEAaq_get_data(pollutant = c("PM10", "PM2.5"),
#'   quadrant = list(c(11.5, 46.5), c(10.5, 46)),
#'   from = 2021, to = 2021, verbose = TRUE)
#' }
#' @export

EEAaq_get_data <- function(zone_name = NULL, NUTS_level = NULL, pollutant = NULL, from = NULL, to = NULL, ID = FALSE, quadrant = NULL, polygon = NULL, verbose = TRUE) {

  `%>%` <- dplyr::`%>%`


  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }




  tictoc::tic()
  stopifnot("Specify either the zone_name, the quadrant or a polygon (sfc_POLYGON object)" = as.numeric(!is.null(polygon))+as.numeric(!is.null(quadrant))+as.numeric(!is.null(zone_name)) == 1)
  stopifnot("You need to specify both the zone_name and the NUTS_level" = (!is.null(zone_name) & !is.null(NUTS_level)) | (is.null(zone_name) & is.null(NUTS_level)))






  #Parametri from e to accettabili
  stopifnot("The parameters 'from' and 'to' must be of the same format: either the year or the date in yyyy-mm-dd format." = class(from) == class(to))

  if(verbose ==  T) {
    cat(paste0("Download preparation started at ", Sys.time(), "\n"))
  }



  #Download dei dataset NUTS e LAU

  temp <- tempfile()
  res <- curl::curl_fetch_disk("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/LAU.rds", temp)
  if(res$status_code == 200) {
    LAU <- readRDS(temp)
  } else {
    stop("The internet resource is not available at the moment, try later.
       If the problem persists, please contact the package maintainer.")
  }


  temp <- tempfile()
  res <- curl::curl_fetch_disk("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/NUTS.rds", temp)
  if(res$status_code == 200) {
    NUTS <- readRDS(temp)
  } else {
    stop("The internet resource is not available at the moment, try later.
       If the problem persists, please contact the package maintainer.")
  }

  stations <- EEAaq_get_stations()


  #Verifico che gli inquinanti inseriti siano disponibili
  #pollutants <- get_AQ_pollutants()
  "%notin%" <- Negate("%in%")
  if(!is.null(pollutant)) {
    if(!is.character(pollutant)) {
      pollutant <- pollutants %>% dplyr::filter(.data$Code %in% pollutant) %>% dplyr::pull(.data$Notation)
    }
    if(sum(pollutant %notin% pollutants$Notation) >= 1) {
      stop("One or more pollutants' code are not available")
    }
  }

  #Se ID = T: da id a NAME_LATN
  if(!is.null(NUTS_level)) {
    if(ID == T & NUTS_level == "LAU") {
      zone_name <- LAU %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LAU_ID %in% zone_name) %>% dplyr::pull(.data$LAU_NAME)
    } else if(ID == T & NUTS_level != "NUTS0") {
      zone_name <- NUTS %>% sf::st_drop_geometry() %>% dplyr::filter(.data$LEVL_CODE == code_extr(NUTS_level) & .data$NUTS_ID %in% zone_name) %>% dplyr::pull(.data$NAME_LATN)
    }
  }

  #Se NUTS_level = NUTS0: da nome a id:
  if(!is.null(NUTS_level)) {
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
            zone[i] <- NUTS %>% dplyr::filter(.data$LEVL_CODE == 0 & .data$NAME_LATN %in% zone_name[i]) %>% dplyr::pull(.data$NUTS_ID)
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


  #Selezione delle stazioni d'interesse
  if(!is.null(zone_name)) {
    if(NUTS_level != "NUTS0") {
      if(sum(zone_name %notin% dplyr::pull(stations, NUTS_level)) == length(zone_name)) {
        stop("All the selected zones are not available.")
      } else if(sum(zone_name %notin% dplyr::pull(stations,NUTS_level)) <= length(zone_name) & sum(zone_name %notin% dplyr::pull(stations,NUTS_level)) > 0) {
        warning("One or more of the selected zones are not available.")
        rows <- stations[dplyr::pull(stations, get(NUTS_level)) %in% zone_name,]
      } else {
        rows <- stations[dplyr::pull(stations, get(NUTS_level)) %in% zone_name,]
      }
    } else if(NUTS_level == "NUTS0") {
      rows <- stations[dplyr::pull(stations, .data$ISO) %in% zone_name,]
    }
  } else if(!is.null(quadrant) & length(quadrant) == 2) {
    angles = data.frame(lon = c(quadrant[[1]][1], quadrant[[2]][1]), lat = c(quadrant[[1]][2], quadrant[[2]][2]))
    polygon <- sf::st_as_sfc(sf::st_bbox(sf::st_as_sf(angles, coords = c("lon", "lat"),
                                          crs = 4326)))
    stations <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326)
    ind <- sf::st_intersects(dplyr::pull(stations[,"geometry"]), polygon, sparse = T)
    rows <- stations[as.logical(apply(as.matrix(ind), 1, sum)),]

  } else if(!is.null(quadrant) & length(quadrant) > 2) {
    lon <- vector()
    lat <- vector()
    for (i in 1:length(quadrant)) {
      lon[i] <- quadrant[[i]][1]
      lat[i] <- quadrant[[i]][2]
    }
    angles <- data.frame(lon = lon, lat = lat)
    polygon <- sf::st_convex_hull(sf::st_union(sf::st_as_sf(angles, coords = c("lon", "lat"), crs = 4326)))
    stations <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326)
    ind <- sf::st_intersects(dplyr::pull(stations[,"geometry"]), polygon, sparse = T)
    rows <- stations[as.logical(apply(as.matrix(ind), 1, sum)),]
  } else if(!is.null(polygon)) {
    stations <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326)
    ind <- sf::st_intersects(dplyr::pull(stations[,"geometry"]), polygon, sparse = T)
    rows <- stations[as.logical(apply(as.matrix(ind), 1, sum)),]
  }

  #Errore se non vengono trovate stazioni
  if(nrow(rows) == 0) {
    stop("No station found in the specified zone")
  }


  #Se sono state indicate le date precise seleziono gli anni di interesse per cui scaricare i dati
  #(Il filtraggio successivo deve essere effettuato dopo il download)
  if(is.character(from) & is.character(to)) {
    start = lubridate::ymd(from)
    end = lubridate::ymd(to)
    from = lubridate::year(start)
    to = lubridate::year(end)
  } else {
    start = NULL
    end = NULL
  }

  temp <- tempfile()
  res <- curl::curl_fetch_disk("https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm", temp)
  if(res$status_code != 200) {
    stop("The internet resource is not available at the moment, try later.
       If the problem persists, please contact the package maintainer.")
  }


  #introduzione dell'URL uguale per qualsiasi combinazione di parametri
  intro <- "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?"

  #Funzione che genera gli URL per il download dei dati

  URL_gen <- function(sampling_point = NULL, from= NULL, to = NULL,
                      time_coverage = "Year") {
    intro <- "https://fme.discomap.eea.europa.eu/fmedatastreaming/AirQualityDownload/AQData_Extract.fmw?"
    URL <- paste0(intro,"CountryCode=","&CityName=","&Pollutant=",
                  "&Year_from=",from,"&Year_to=",to,"&EoICode=","&Samplingpoint=", sampling_point,"&Source=All&Output=TEXT&UpdateDate=&",
                  "TimeCoverage=Year", sep = "")
    return(URL)
  }


  #Filtro i sensori per cui scaricare i dati in base agli inquinanti richiesti e verifico che gli inquinanti
  #siano disponibili per la zona selezionata
  if(!is.null(pollutant)) {
    if(sum(pollutant %notin% dplyr::pull(rows, .data$AirPollutant)) >= 1 & sum(pollutant %notin% dplyr::pull(rows, .data$AirPollutant)) < length(pollutant)) {
      warning(paste0("The pollutants ", paste(pollutant[pollutant %notin% dplyr::pull(rows, .data$AirPollutant)], collapse = ", "), " are not available for the selected zone"), immediate. = T)
      rows <- rows %>% dplyr::filter(.data$AirPollutant %in% pollutant) %>% dplyr::pull(.data$SamplingPointId)
      links <- URL_gen(sampling_point = rows ,from = from, to = to)
    } else if(sum(pollutant %notin% dplyr::pull(rows, .data$AirPollutant)) == length(pollutant)) {
      stop("The selected pollutants are not available for this zone")
    } else {
      rows <- rows %>% dplyr::filter(.data$AirPollutant %in% pollutant) %>% dplyr::pull(.data$SamplingPointId)
      links <- URL_gen(sampling_point = rows ,from = from, to = to)
    }
    #Se non sono specificati inquinanti non servono altri filtraggi
  } else if(is.null(pollutant)) {
    rows <- rows %>% dplyr::pull(.data$SamplingPointId)
    links <- URL_gen(sampling_point = rows, from = from, to = to)
  }

  #Download dei link che contengono i dati
  if(verbose ==  T) {
    cat(paste0("Download preparation ended at ", Sys.time(), "\n"))
    cat(paste0("\n", "Downloading data started at ", Sys.time(), "\n"))
  }
  url.list <- lapply(links, readr::read_csv, show_col_types = F, col_names = F)
  #download finale dei dati
  data <- dplyr::bind_rows(lapply(unlist(url.list), readr::read_csv, show_col_types = F))

  if(verbose ==  T) {
    cat(paste0("Downloading data ended at ", Sys.time(), "\n"))
  }

  #Se non sono presenti dati per i parametri specificati:
  if(sum(dim(data) == c(0,0)) >= 1) {
    stop("No data is available for the specified parameters")
  }

  #Converto le colonne delle date nel formato UTC e filtro in base all'intervallo di tempo specificato nei parametri
  data$DatetimeBegin <- lubridate::ymd_hms(data$DatetimeBegin)
  data$DatetimeEnd <- lubridate::ymd_hms(data$DatetimeEnd)
  if(!is.null(start) & !is.null(end)) {
    data <- data %>% dplyr::filter(.data$DatetimeBegin >= start & .data$DatetimeEnd <= end)
  } else {
    data <- data %>% dplyr::filter(lubridate::year(.data$DatetimeBegin) %in% from:to)
  }


  nomi_sta <- stations %>% dplyr::filter(.data$AirQualityStationEoICode %in% unique(data$AirQualityStationEoICode)) %>%
    dplyr::distinct(.data$AirQualityStationEoICode, .data$AirQualityStationName)

  #Se nell'area selezionata non ci sono stazioni per uno degli inquinanti scelti:
  if(sum(pollutant %notin% unique(data$AirPollutant)) >= 1) {
    warning(paste("No station is available for the pollutants", pollutant[which(pollutant %notin% unique(data$AirPollutant))], sep = " "))
    pollutant <- unique(data$AirPollutant)
  }

  data <- data %>% dplyr::select("AirQualityStationEoICode", "AirPollutant", "Concentration", "AveragingTime", "DatetimeBegin", "DatetimeEnd") %>%
    tidyr::pivot_wider(names_from = "AirPollutant", values_from = "Concentration", values_fn = {mean}) %>%
    dplyr::left_join(nomi_sta, by = "AirQualityStationEoICode") %>%
    dplyr::relocate("AirQualityStationName", .after = "AirQualityStationEoICode") %>%
    dplyr::relocate(dplyr::where(is.numeric), .after = "AirQualityStationName") %>%
    dplyr::arrange(.data$AirQualityStationEoICode, .data$DatetimeBegin)

  if(!is.null(zone_name)) {
    attr(data, "class") <- c("EEAaq_df", "tbl_df", "tbl", "data.frame")
    attr(data, "NUTS_level") <- NUTS_level
    attr(data,"zone_name") <- zone_name
    attr(data, "pollutants") <- pollutant
  } else if(!is.null(quadrant) | !is.null(polygon)) {
    attr(data, "class") <- c("EEAaq_df_sfc", "tbl_df", "tbl", "data.frame")
    attr(data,"zone_geometry") <- polygon
    attr(data, "pollutants") <- pollutant
  }

  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  tot_time <- lubridate::seconds_to_period(tot_time)
  if(verbose == T){
    cat(paste0("Total download time: ", tot_time, "\n"))
  }

  closeAllConnections()

  return(data)
}




#' Extract numeric NUTS code from the chracter
#'
#' This function take as argument the chracter NUTS or LAU code ("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU")
#' and mutate it in a numerical hierarchical coding.
#' @param level Character value of the NUTS level
#' @return integer corresponding to the relative NUTS level or LAU.


code_extr <- function(level) {
  code <- vector()
  for (i in level) {
    if(nchar(i) == 5) {
      code <- c(code, as.numeric(substr(i, 5,5)))
    } else {
      code <- c(code, 4)
    }
  }
  return(sort(code))
}





