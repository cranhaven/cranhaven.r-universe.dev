#' Download fishing effort map data
#'
#' Download a data.frame of kw fishing hours by c-square and gear
#' category averaged over 4 years.
#'
#' @param ecoregion ICES ecoregion (see details)
#' @param year which year to select (see details)
#' @param convert2sf logical, default FALSE, should an simple features object
#'   be returned if the \code{sf} package is installed?
#' 
#' @return a data.frame with a WKT column for the c-square polygons
#'
#' @details
#'
#' The spatial data.frame contains average annual mega Watt fishing hours,
#' averaged over 4 years.
#' 
#' Available ecoregions are given in the description field of the ICES
#' ecoregion vocabulary <http://vocab.ices.dk/?ref=1414>
#' 
#' @examples 
#' \dontrun{
#' # requires authorization
#' ns_effort_map <- get_effort_map("Greater North Sea", convert2sf = TRUE)
#' plot(
#'   ns_effort_map[ns_effort_map$fishing_category_FO == "Otter","mw_fishinghours"], 
#'   border = FALSE, logz = TRUE
#' )
#' }
#' 
#' @importFrom utils URLencode
#' @export
get_effort_map <- function(ecoregion, year = NULL, convert2sf = FALSE) {
  
  check_ecoregion(ecoregion)
  
  url <-
    vms_api(
      glue("fisheriesoverviews/effortmap/{URLencode(ecoregion)}"),
      year = 2022
    )

  out <- vms_get(url, use_token = TRUE)
  
  if (convert2sf) {
    convert_df2sf(out)
  } else {
    out
  }
}
