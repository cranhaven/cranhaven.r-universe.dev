#' Download Pre-Defined collection
#'
#' Download a predefined collection of time series.
#' @inheritParams param_defs
#' @param collection_name The name of the collection you wish to download. For a list of available collections, go to ["list_available_collections"]
#' @param show_progress If collection to true, shows a progress bar of the data being downloaded.
#' @examples
#' try(get_collection("ds_kmi_mixed_freq",show_progress = TRUE))
#' @import httr
#' @import jsonlite
#' @export
get_collection <- function(collection_name, api_key = NULL, show_progress = FALSE) {
  # Build request URL
  url <- "https://datenservice.kof.ethz.ch/api/v1/%s/collections/%s"

  query = list(
    df = "Y-m-d"
  )

  if(!is.null(api_key)) {
    url <- sprintf(url, "main", collection_name)
    query$apikey = api_key
  } else {
    url <- sprintf(url, "public", collection_name)
  }

  # Call the API
  if(show_progress) {
    response <- GET(url, progress(), query = query)
  } else {
    response <- GET(url, query = query)
  }

  response <- validate_response(response)

  data <- fromJSON(content(response, as = "text"))

  lapply(data, .json_to_ts)
}
