#' Check if a given object is an \code{EEAaq_df} class object
#'
#' Given an object as input, \code{is_EEAaq_df} verify that the given object belongs
#' to the \code{EEAaq_df} class.
#' @param data the object for which verify the if it belongs to the \code{EEAaq_df} class.
#' @return logical value (T ot F). If \code{TRUE} the object given in input is an \code{EEAaq_df} object.
#' If \code{FALSE} the object doesn't belong to the \code{EEAaq_df} class.
#' @examples
#' \donttest{
#' ### Download PM10 data for the province (NUTS-3) of Milano (Italy)
#' # from January 1st to January 31st, 2023
#' `%>%` <- dplyr::`%>%`
#' IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
#' IDstations <- IDstations %>%
#'                 dplyr::filter(NUTS3 %in% c("Milano")) %>%
#'                 dplyr::pull(AirQualityStationEoICode) %>%
#'                 unique()
#' data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
#'                        from = "2023-01-01", to = "2023-01-31", verbose = TRUE)
#'
#' ### Check if the imported object belongs to the EEAaq_df class
#' is_EEAaq_df(data = data)
#' }
#'
#' @export



is_EEAaq_df <- function(data) {


  if("EEAaq_df" %in% attributes(data)$class | "EEAaq_df_sfc" %in% attributes(data)$class) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
