#' Taxi Availability
#'
#' This functions calls upon the taxi availability API from data.gov.sg
#' and returns a data frame of the locations of all availible taxis.
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords taxi
#'
#' @return A dataframe containing the longitude and latitude values of availible taxis.
#'
#' @export
#' @examples
#' taxi_availability()
#' taxi_availability(date = "2019-08-07T09:30:00")
#' taxi_availability(date = "2018-06-05T13:45:00")

taxi_availability = function(date_time = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "transport/taxi-availability",
                       input_date = date_time,
                       summary = FALSE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  if (length(content.output$features[[1]]$geometry$coordinates) == 0) {
    message("No data returned from API.")
    return(NULL)
  }

  # Extracting Data Frame
  message("Timestamp: ", content.output$features[[1]]$properties$timestamp)
  message("Availible Taxis: ", content.output$features[[1]]$properties$taxi_count)

  taxi_locations = as.data.frame(data.table::rbindlist(content.output$features[[1]]$geometry$coordinates))
  colnames(taxi_locations) = c("long", "lat")

  return(taxi_locations)

}

