#' Export and save an \code{EEAaq_df} class object
#'
#' \code{EEAaq_export} export an \code{EEAaq_df} class object as a \emph{.csv} or a \emph{.txt} file.
#' @param data an \code{EEAaq_df} class object.
#' @param filepath character string giving the file path
#' @param format character string giving the format of the file. It must be one of 'csv' and 'txt'.
#'
#' @return No return value, called for side effects.
#' @examples
#' \donttest{
#' ### Download PM10 data for the province (NUTS-3) of Milano
#' ## (Italy) from January 1st to January 31st, 2023
#' `%>%` <- dplyr::`%>%`
#' IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
#' IDstations <- IDstations %>%
#'                 dplyr::filter(NUTS3 %in% c("Milano")) %>%
#'                 dplyr::pull(AirQualityStationEoICode) %>%
#'                 unique()
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
#'                        from = "2024-01-01", to = "2025-01-31", verbose = TRUE)
#'
#' ### Export data to csv file
#' temp <- tempdir()
#' filepath <- paste0(temp, "/data.csv")
#' EEAaq_export(data = data, filepath = filepath, format = "csv")
#' }
#' @export

EEAaq_export <- function(data, filepath, format) {

  `%>%` <- dplyr::`%>%`
  "%notin%" <- Negate("%in%")

  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }

  # Download dei dataset NUTS e LAU
  LAU <- EEAaq_get_dataframe(dataframe = "LAU")
  NUTS <- EEAaq_get_dataframe(dataframe = "NUTS")

  # Se l'oggetto non e' di classe EEAaq_df errore
  stopifnot("The given object for the parameter data is not an 'EEAaq_df' class object" =
              "EEAaq_df" %in% class(data) | "EEAaq_df_sfc" %in% class(data))

  # Save data.frame
  if (format %in% c("csv","txt")) {
    readr::write_csv(x = data, file = filepath)
  }

}

