#' List Available Set Names and Their Description
#'
#' This function is deprecated. Use \code{\link{list_available_collections}} instead.
#' Sets are pre-defined lists of time series. Sets are the convenient alternative to concatenating many series in an URL.
#' @inheritParams param_defs
#' @return A data frame with the names, descriptions and public availability of datasets. These sets can be downloaded via get_dataset.
#' @import httr
#' @import jsonlite
#' @export
list_available_sets <- function(api_key = NULL) {
.Deprecated("list_available_collections")
}

