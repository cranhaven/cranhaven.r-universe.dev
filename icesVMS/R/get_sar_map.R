#' Download swept area ratio map data
#'
#' Download a data.frame of surface and subsurface swept area
#' ratio by c-square for a given ICES ecoregion.
#'
#' @param ecoregion ICES ecoregion
#' @param year which year to select (see details)
#' @param nyears the number of years to take an average over
#' @param convert2sf logical, default FALSE, should an simple features object
#'   be returned if the \code{sf} package is installed?
#' 
#' @return a data.frame with a WKT column for the c-square polygons
#'
#' @details
#'
#' The spatial data.frame contains average annual surface-swept-area-ratio
#' and subsurface-swept-area-ratio averaged over 4 years by default.  If year
#' is not specified (NULL) then the present year - 1 is assumed.
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' sar_map <- get_sar_map("Celtic Seas", 2021, convert2sf = TRUE)
#' plot(sar_map["surface_sar"], border = FALSE, logz = TRUE)
#' }
#' 
#' @export
get_sar_map <- function(ecoregion, year = NULL, nyears = NULL, convert2sf = FALSE) {
  check_ecoregion(ecoregion)

  url <-
    vms_api(
      glue("fisheriesoverviews/sarmap/{URLencode(ecoregion)}"),
      year = year, nyears = nyears
    )

  out <- vms_get(url, use_token = TRUE)

  if (convert2sf) {
    convert_df2sf(out)
  } else {
    out
  }
}
