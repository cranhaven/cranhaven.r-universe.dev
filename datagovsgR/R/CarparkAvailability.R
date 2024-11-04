#' Carpark Availability
#'
#' This functions calls upon the carpark availability API from data.gov.sg
#' and processes the returning page.
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords carpark
#'
#' @return A dataframe containing the carpark id, type, last update, total lots, and current lots.
#'
#' @export
#' @examples
#' \donttest{
#' carpark_availability()
#' carpark_availability(date = "2019-06-05T10:10:10")
#' carpark_availability(date = "2018-12-01T19:32:56")
#' }

carpark_availability = function(date_time = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "transport/carpark-availability",
                       input_date = date_time,
                       summary = FALSE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  # Extracting Data Frame
  message("Closest timestamp: ", content.output$items[[1]]$timestamp)

  carpark_availability = lapply(1:length(content.output$items[[1]]$carpark_data), function(x){

    data.frame(id = content.output$items[[1]]$carpark_data[x][[1]]$carpark_number,
               type = content.output$items[[1]]$carpark_data[x][[1]]$carpark_info[[1]]$lot_type,
               last_update = content.output$items[[1]]$carpark_data[x][[1]]$update_datetime,
               total_lots = as.integer(content.output$items[[1]]$carpark_data[x][[1]]$carpark_info[[1]]$total_lots),
               availability_lots = as.integer(content.output$items[[1]]$carpark_data[x][[1]]$carpark_info[[1]]$lots_available),
               stringsAsFactors = FALSE)

  })

  carpark_availability = dplyr::bind_rows(carpark_availability)

  return(carpark_availability)

}

