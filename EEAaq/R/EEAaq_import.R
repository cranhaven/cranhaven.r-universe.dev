#' Reverse function of \code{EEAaq_export}. Reads an \code{EEAaq_df} object from a .txt or .csv
#' file saved through \code{\link{EEAaq_export}}.
#'
#' @param file_data file path of the 'csv' or 'txt' file containing the air quality data to import.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \donttest{
#' `%>%` <- dplyr::`%>%`
#' ### Download PM10 data for the province (NUTS-3) of Milano (Italy)
#' ## from January 1st to January 31st, 2023
#' IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
#' IDstations <- IDstations %>%
#'                 dplyr::filter(NUTS3 %in% c("Milano")) %>%
#'                 dplyr::pull(AirQualityStationEoICode) %>%
#'                 unique()
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
#'                        from = "2023-01-01", to = "2023-01-31", verbose = TRUE)
#'
#' ### Export data to csv file
#' temp <- tempdir()
#' filepath <- paste0(temp, "/data.csv")
#' EEAaq_export(data = data, filepath = filepath, format = "csv")
#'
#' ### Import the EEAaq_df object saved in the previous code line
#' EEAaq_import(file_data = filepath)
#' }
#'
#' @export



EEAaq_import <- function(file_data) {

  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.") #se false, stop interrompe esecuzione
  }

  `%>%` <- dplyr::`%>%`
  #Lettura dei dati
  if(substr(file_data, nchar(file_data)-2, nchar(file_data)) == "csv") {
    data <- readr::read_csv(file_data, show_col_types = F)
  } else if(substr(file_data, nchar(file_data)-2, nchar(file_data)) == "txt") {
    data <- utils::read.table(file_data)
    data <- dplyr::as_tibble(data)
  }

  #Aggiungo la classe e gli attributi dell'oggetto EEAaq_df_sfc
  attr(data, "class") <- c("EEAaq_df_sfc", "tbl_df", "tbl", "data.frame")
  pollutant <- setdiff(colnames(data), c("AirQualityStationEoICode", "AirQualityStationName", "AveragingTime", "DatetimeBegin", "DatetimeEnd"))
  attr(data, "pollutants") <- pollutant

  return(data)

}
