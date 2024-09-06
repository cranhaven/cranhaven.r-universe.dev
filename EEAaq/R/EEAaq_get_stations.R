#' Download EEA measurement station information dataset
#'
#' Download the updated dataset from EEA, containing measurement station information. For further information about the variables
#' see \code{\link{stations}}.
#' @param byStation Logic value (T or F). If \code{TRUE} the dataset is organized by station (one row for each measurement station).
#' If \code{FALSE} the dataset is organized by sampling point. Each station have multiple sampling points.
#' @param complete Logic value (T or F). If \code{TRUE}, the dataset contains all the variables given by the EEA.
#' If \code{FALSE} the dataset contains only a few variables, the most importants. For further details about the variables, see \code{\link{stations}}.
#' @return A tibble containing the stations information. Further details available here \code{\link{stations}}.
#' @examples
#' \donttest{EEAaq_get_stations(byStation = FALSE, complete = TRUE)}
#' @export

EEAaq_get_stations <- function(byStation = FALSE, complete = TRUE) {

  `%>%` <- dplyr::`%>%`


  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }


  #download dei metadati:
  temp <- tempfile()
  res <- curl::curl_fetch_disk("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/stations_reshaped.rds", temp)
  if(res$status_code == 200) {
    stations <- readRDS(temp)
  } else {
    stop("The internet resource is not available at the moment, try later.
       If the problem persists, please contact the maintainer.")
  }

  #temp <- tempfile()
  #utils::download.file("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/stations_reshaped.rds", temp, quiet = T)
  #stations <- readRDS(temp)


  if(byStation == T) {
    stations <- stations %>% dplyr::distinct(.data$AirQualityStationEoICode, .keep_all = T) %>%
      dplyr::select("AirQualityStationEoICode", "AirQualityStationNatCode", "AirQualityStationName", "AirQualityStationArea", "AirQualityStationType", "Longitude", "Latitude", "Altitude", "ISO", "NUTS1", "NUTS1_ID",
                    "NUTS2", "NUTS2_ID", "NUTS3", "NUTS3_ID", "LAU", "LAU_ID", "AirQualityNetwork", "AirQualityNetworkName",
                    "Timezone")
    if(complete == F) {
      stations <- stations %>% dplyr::select("AirQualityStationEoICode", "AirQualityStationNatCode", "AirQualityStationName", "AirQualityStationArea", "AirQualityStationType", "Longitude", "Latitude", "Altitude", "ISO", "NUTS1",
                                             "NUTS2", "NUTS3", "LAU")
    }
  } else if(byStation == F & complete == F) {
    stations <- stations %>% dplyr::select("SamplingPointId", "AirQualityStationEoICode", "AirQualityStationName", "AirPollutant", "OperationalActivityBegin", "OperationalActivityEnd", "ISO", "NUTS1", "NUTS2", "NUTS3", "LAU", "AirQualityStationArea", "AirQualityStationType", "Longitude", "Latitude", "Altitude", "Timezone")
  }


  return(stations)

}
