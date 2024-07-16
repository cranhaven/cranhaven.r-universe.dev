#' Download swept area ratio, landings and value map data
#'
#' Download a data.frame of surface and subsurface swept area
#' ratio by c-square for a given ICES ecoregion, year and gear code.
#'
#' @param ecoregion ICES ecoregion
#' @param year which year to select
#' @param convert2sf logical, default FALSE, should an simple features object
#'   be returned if the \code{sf} package is installed?
#' 
#' @return a data.frame with a WKT column for the c-square polygons
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' data2 <- get_wgfbit_data2("Celtic Seas", 2021, convert2sf = TRUE)
#' plot(data2[data2$lE_MET_level6 == "OTB_DEF_70-99_0_0", "total_weight"], border = NA, logz = TRUE)
#' }
#'
#' @export
get_wgfbit_data2 <- function(ecoregion, year, convert2sf = FALSE) {
  
  check_ecoregion(ecoregion)
  
  url <-
    vms_api(
      glue("wgfbit/dataset2/{URLencode(ecoregion)}/{year}")
    )
  
  out <- vms_get(url, use_token = TRUE)

  if (convert2sf) {
    convert_df2sf(out)
  } else {
    out
  }
}
