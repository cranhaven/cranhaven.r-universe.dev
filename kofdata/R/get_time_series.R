#' Get Time Series form KOF Datenservice API
#'
#' Download time series data from the KOF web API.
#' 
#' @inheritParams param_defs
#' @param ts_keys A vector of timeseries keys
#' @param show_progress If set to true, shows a progress bar of the data being downloaded.
#' year-month-day, otherwise only year and month.
#' @import httr
#' @import jsonlite
#' @examples
#' try(get_time_series("kofbarometer"))
#' @export
get_time_series <- function(ts_keys, api_key = NULL,
                            show_progress = FALSE) {

  # Build request URL
  keys <- paste(ts_keys, collapse=",")

  url <- "https://datenservice.kof.ethz.ch/api/v1/%s/ts"

  query <- list(
    keys = keys,
    df = "Y-m-d"
  )

  if(!is.null(api_key)) {
    url <- sprintf(url, "main")
    query$apikey <- api_key
  } else {
    url <- sprintf(url, "public")
  }

  # Call the API
  if(show_progress) {
    response <- GET(url, progress(), query = query)
  } else {
    response <- GET(url, query = query)
  }

  response <- validate_response(response)

  data <- fromJSON(content(response, as="text"))

  lapply(data, .json_to_ts)
}
