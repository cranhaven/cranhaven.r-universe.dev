#' Build a WKT string from latitude and longitude
#'
#' utility to build a WKT (Well Known Text) polygon string
#' for a c-square
#'
#' @param lat latitude
#' @param lon longitude
#'
#' @return a string in WKT format
#'
#' @examples
#'
#' wkt_csquare(55, 0.1)
#'
#' @importFrom glue glue
#'
#' @export
wkt_csquare <- function(lat, lon) {
  glue(
    "POLYGON(({lon - 0.025} {lat - 0.025},",
    "{lon - 0.025} {lat + 0.025},",
    "{lon + 0.025} {lat + 0.025},",
    "{lon + 0.025} {lat - 0.025},",
    "{lon - 0.025} {lat - 0.025}))"
  )
}