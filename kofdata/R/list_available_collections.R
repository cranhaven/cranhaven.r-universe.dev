#' List Available collection Names and Their Description
#'
#' collections are pre-defined lists of time series. collections are the convenient alternative to concatenating many series in an URL.
#' @inheritParams param_defs
#' @return A data frame with the names, descriptions and public availability of collections. These collections can be downloaded via get_collection.
#' @import httr
#' @import jsonlite
#' @examples
#' try(list_available_collections())
#' @export
#' @aliases list_available_collections
list_available_collections <- function(api_key = NULL) {
  # Build request URL
  url <- sprintf("https://datenservice.kof.ethz.ch/api/v1/%s/collections/", ifelse(is.null(api_key), "public", "main"))
  query <- list(apikey = api_key)

  response <- GET(url, query = query)

  if(response$status_code == 200) {
    ret <- fromJSON(content(response, as = "text"))
    descriptions <- sapply(ret, '[[', "description")
    descriptions[sapply(descriptions, is.null)] <- NA
    data.frame(
      collection_name = names(ret),
      collection_description = unlist(descriptions),
      is_public = sapply(ret, '[[', "is_public"),
      stringsAsFactors = FALSE
    )
  } else {
    stop(sprintf("An error occurred when calling the api:\nStatus: %d\nContent:%s", response$status_code, content(response, as = "text")))
  }
}
