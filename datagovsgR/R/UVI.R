#' UVI(Ultra-violet Index)
#'
#' This functions calls upon the UVI API from data.gov.sg
#' and returns a data frame of the different measures of the UVI across
#' Singapore and returns the closest UVI value presently and for the past
#' few hours. This data provided by the API is updated hourly.
#'
#' @param date_time Defaults to current (SGD) time. Format: YYYY-MM-DDTHH:MM:SS
#'
#' @keywords UVI
#'
#' @return A dataframe containing the current and past hourly UVI
#'
#' @export
#' @examples
#' uvi()
#' uvi(date = "2022-09-03T16:45:23")
#' uvi(date = "2021-12-06T11:01:55")

uvi = function(date_time = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "environment/uv-index",
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

  uvi = as.data.frame(dplyr::bind_rows(content.output$items[[1]]$index))

  return(uvi)

}

