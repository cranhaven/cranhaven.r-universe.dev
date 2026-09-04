#' List All Keys in a Set
#'
#' List the keys of all time series in a set. To learn more about specific keys, use get_metadata.
#' @inheritParams param_defs
#' @param setname The name of the set
#' @return If a single set name is provided, a vector of time series keys.
#' If multiple set names are provided, a list of vectors of time series keys.
#' @import httr
#' @import jsonlite
#' @export
list_keys_in_set <- function(setname, api_key = NULL) {
  .Deprecated("list_keys_in_collection")
}
