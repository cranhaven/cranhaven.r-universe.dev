#' Download EEA measurement station information dataset
#'
#' Download the updated dataset from EEA, containing measurement station information. For further information about the variables
#' see \code{stations}.
#' @param byStation Logic value (T or F). If \code{TRUE} the dataset is organized by station (one row for each measurement station).
#' If \code{FALSE} the dataset is organized by sampling point. Each station have multiple sampling points.
#' @param complete Logic value (T or F). If \code{TRUE}, the dataset contains all the variables given by the EEA.
#' If \code{FALSE} the dataset contains only a few variables, the most importants. For further details about the variables, see \code{stations}.
#' @return A tibble containing the stations information. Further details available here \code{stations}.
#'
#' @details
#' Note that, for very small towns or certain countries, such as Turkey or Albania, data may not currently be available in the dataset. This limitation reflects the data unavailability at the the EEA Air Quality Viewer <https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.b2g.AirQualityStatistics>.
#'
#' @examples
#' \donttest{
#' EEAaq_get_stations(byStation = TRUE, complete = TRUE)
#' }
#' @export

EEAaq_get_stations <- function(byStation = TRUE, complete = TRUE) {

  `%>%` <- dplyr::`%>%`

  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }

  stations <- EEAaq_get_dataframe(dataframe = "stations")

  stations <- stations %>%
    dplyr::mutate(OperationalActivityBegin = lubridate::dmy_hms(.data$OperationalActivityBegin),
                  OperationalActivityEnd = lubridate::dmy_hms(.data$OperationalActivityEnd))

  if(byStation == T) {
    stations <- stations %>%
      dplyr::distinct(.data$AirQualityStationEoICode, .keep_all = T)
    if(complete == F) {
      stations <- stations %>%
        dplyr::select("AirQualityStationEoICode", "AirQualityStationNatCode", "AirQualityStationName", "AirQualityStationArea", "AirQualityStationType", "Longitude", "Latitude", "Altitude", "ISO",
                      "NUTS1", "NUTS2", "NUTS3", "LAU_NAME")
    }
  } else if(complete == F) {
    stations <- stations %>%
      dplyr::select("SamplingPointId", "AirQualityStationEoICode", "AirQualityStationName", "AirPollutant", "OperationalActivityBegin", "OperationalActivityEnd", "ISO", "NUTS1", "NUTS2", "NUTS3", "LAU_NAME", "AirQualityStationArea", "AirQualityStationType", "Longitude", "Latitude", "Altitude", "Timezone")
  }


  return(stations)
}
