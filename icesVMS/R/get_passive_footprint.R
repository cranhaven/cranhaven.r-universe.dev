#' Download passive fishing gear footprint
#'
#' Download a data.frame of presence of fishing by c-square and year
#' for passive fishing gears (see details).
#'
#' @param ecoregion ICES ecoregion
#' @param year which year to select
#' @param metier_level4 optional gear code (metier level 4) ("FPO")
#' @param datacall integer year giving which data call year to inquire about.
#'   If NULL returns the a summary of the most recent approved data.
#' @param convert2sf logical, default FALSE, should an simple features object
#'   be returned if the \code{sf} package is installed?
#'
#' @return a data.frame with a WKT column for the c-square polygons
#'
#' @details
#'
#' Passive gears defined as all gears registered under
#' the metier level 4 codes, FPO (fishing pots), LLS (long lines) and
#' GNS (set gill nets), with the exclusion of metier level 5 codes
#' within the GNS category: GNS_SPF and GNS_LPF (set gill nets
#' targeting small and large pelagic fish).
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' footprint_map <- get_passive_footprint("Celtic Seas", 2021, convert2sf = TRUE)
#' plot(footprint_map["ecoregion"], border = FALSE)
#' }
#' 
#' @export
get_passive_footprint <- function(ecoregion, year, metier_level4 = NULL, datacall = NULL, convert2sf = FALSE) {
  
  check_ecoregion(ecoregion)
  
  url <-
    vms_api(
      glue("footprint/passive/{URLencode(ecoregion)}/{year}"),
      metier_level4 = metier_level4, datacall = datacall
    )

  out <- vms_get(url, use_token = TRUE)
  
  if (convert2sf) {
    convert_df2sf(out)
  } else {
    out
  }
}
