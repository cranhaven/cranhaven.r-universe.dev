#' PM2.5 Summary
#'
#' This functions calls upon the PM2.5 API from data.gov.sg
#' and returns a data frame of the different measures of the PM2.5 across 5
#' different areas in Singapore for each hour. This data provided by the
#' API is updated hourly from NEA.
#'
#' Note that this function is different from the `pm25` function,
#' which returns the PM2.5 measures for a given date and time.
#'
#' @param date Defaults to current (SGD) time. Format: YYYY-MM-DD
#'
#' @keywords PM2.5
#'
#' @return A dataframe containing various PM2.5 measures across 5 corners
#' of Singapore and time of day. Dependent on the data availible, not all
#' results range from 0000 to 2300.
#'
#' @export
#' @examples
#' pm25_summary()
#' pm25_summary(date = "2019-11-08")
#' pm25_summary(date = "2018-01-04")

pm25_summary = function(date = "") {

  # Creating and pulling URL
  URL = parse_api_date(api = "environment/pm25",
                       input_date = date,
                       summary = TRUE)
  output = httr::GET(URL)

  # Error check
  content.output = parse_api_output(output)

  if (length(content.output$items) == 0) {
    stop("No data returned from API.")
  }

  # Extracting Data Frame

  pm25_summary = lapply(1:length(content.output$items), function(x) {

    pm_25summary = cbind(timestamp = content.output$items[[x]]$timestamp,
                         as.data.frame(content.output$items[[x]]$readings$pm25_one_hourly),
                         stringsAsFactors = FALSE)
  })

  pm25_summary = dplyr::bind_rows(pm25_summary)

  return(pm25_summary)

}

