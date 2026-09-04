#' List All Keys in a Collection
#'
#' List the keys of all time series in a collection. To learn more about specific keys, use get_metadata.
#' @inheritParams param_defs
#' @param collectionname The name of the collection
#' @return If a single collection name is provided, a vector of time series keys.
#' If multiple collection names are provided, a list of vectors of time series keys.
#' @import httr
#' @import jsonlite
#' @examples
#' try(list_keys_in_collection("ds_kmi_mixed_freq"))
#' @export
list_keys_in_collection <- function(collectionname, api_key = NULL) {
  url <- "https://datenservice.kof.ethz.ch/api/v1/%s/collections/details/%s"

  keylists <- lapply(collectionname, function(collection){
    response <- GET(sprintf(url, ifelse(is.null(api_key), "public", "main"), collection),
                    query = list(apikey = api_key))

    if(response$status_code == 200) {
      fromJSON(content(response, as = "text"))$keys
    } else if(response$status_code == 404){
      stop(sprintf("Could not find collection %s!", collectionname))
    } else {
      stop(sprintf("An error occurred when calling the api:\nStatus: %d\nContent:%s", response$status_code, content(response, as = "text")))
    }
  })

  names(keylists) <- collectionname

  if(length(collectionname) > 1) {
    keylists
  } else {
    keylists[[1]]
  }
}
