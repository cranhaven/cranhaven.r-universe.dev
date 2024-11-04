#' PM2.5
#'
#' This functions calls upon the PM2.5 API from data.gov.sg
#' and returns a data frame of the different measures of PM2.5 across 5
#' different areas in Singapore. This data provided by the API is updated
#' hourly from NEA.
#'
#' Note that this function is different from the `pm25_summary` function,
#' which returns the PM2.5 measures for a given day.
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords PM2.5
#'
#' @return A dataframe containing various PM2.5 measures across 5 corners
#' of Singapore
#'
#' @export
#' @examples
#' pm25()
#' pm25(date_time = "2019-11-08T17:30:00")
#' pm25(date_time = "2018-01-04T09:16:17")

pm25 = function(date_time = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "environment/pm25",
                       input_date = date_time,
                       summary = FALSE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  if (length(content.output$items) == 0) {
    stop("No data returned from API.")
  }

  # Extracting Data Frame
  message("Closest timestamp: ", content.output$items[[1]]$timestamp)

  pm25 = as.data.frame(content.output$items[[1]]$readings$pm25_one_hourly)

  return(pm25)

}

