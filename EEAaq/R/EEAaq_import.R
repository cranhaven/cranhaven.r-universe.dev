#' Reverse function of \code{EEAaq_export}. Reads an \code{EEAaq_df} object
#'
#' Given the file containing the data saved with \code{\link{EEAaq_export}}, and the
#' associated shapefile, \code{EEAaq_read} imports the \code{EEAaq_df} class object.
#' @param file_data file path of the 'csv' or 'txt' file containing the air quality data
#' to import.
#' @param file_shape file path of the shapefile associated to the dataset used in \code{file_data}
#' @return No return value, called for side effects.
#'
#' @examples
#' \donttest{
#' #Download a dataset with the function EEAaq_get_data, which generate an EEAaq_df object.
#' data <- EEAaq_get_data(zone_name = "Milano", NUTS_level = "LAU", pollutant = "PM10",
#'   from = 2021, to = 2021, verbose = TRUE)
#' temp <- tempdir()
#' filepath <- paste0(temp, "/data.csv")
#' #Export the dataset and the associated shape
#' EEAaq_export(data = data, filepath = filepath, format = "csv", shape = TRUE)
#' #Import the EEAaq_df object saved in the previous code line
#' EEAaq_import(file_data = filepath, file_shape = paste0(temp, "/data.shp"))
#' }
#' @export



EEAaq_import <- function(file_data, file_shape) {

  `%>%` <- dplyr::`%>%`
  #Lettura dei dati
  if(substr(file_data, nchar(file_data)-2, nchar(file_data)) == "csv") {
    data <- readr::read_csv(file_data, show_col_types = F)
  } else if(substr(file_data, nchar(file_data)-2, nchar(file_data)) == "txt") {
    data <- utils::read.table(file_data)
    data <- dplyr::as_tibble(data)
  }

  #Lettura dello shape
  polygon <- sf::read_sf(file_shape)



  #Aggiungo la classe e gli attributi dell'oggetto EEAaq_df_sfc
  attr(data, "class") <- c("EEAaq_df_sfc", "tbl_df", "tbl", "data.frame")
  attr(data,"zone_geometry") <- polygon
  pollutant <- pollutant <- setdiff(colnames(data), c("AirQualityStationEoICode", "AirQualityStationName", "AveragingTime", "DatetimeBegin", "DatetimeEnd"))
  attr(data, "pollutants") <- pollutant

  return(data)

}



