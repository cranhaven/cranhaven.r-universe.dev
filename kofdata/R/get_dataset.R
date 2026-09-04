#' Download Pre-Defined Dataset
#'
#' This function is deprecated. Use \code{\link{get_collection}} instead.
#' Download a predefined set of time series.
#' @inheritParams param_defs
#' @param set_name The name of the set you wish to download. For a list of available sets, go to ["list_available_sets"]
#' @param show_progress If set to true, shows a progress bar of the data being downloaded.
#' @import httr
#' @import jsonlite
#' @export
get_dataset <- function(set_name, api_key = NULL, show_progress = FALSE) {
  .Deprecated("get_collection")
  }
