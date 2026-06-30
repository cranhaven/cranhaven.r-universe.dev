#' Convert Finna query object to JSON
#'
#' @param x An object to convert (e.g., a Finna API query or response object).
#'
#'
#' @keywords internal
finna_as_json <- function(x) {
  UseMethod("finna_as_json")
}

#' @rdname finna_as_json
#' @keywords internal
#' @export
finna_as_json.finna_query <- function(x) {
  # If the query has fields like 'filters' or 'facets', ensure they are handled
  if (!is.null(x$filters)) {
    x$filters <- as.list(x$filters)  # Convert filters to list if not already
  }
  if (!is.null(x$facets)) {
    x$facets <- as.list(x$facets)  # Convert facets to list if not already
  }

  # Convert the entire object to JSON using jsonlite
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)
}
