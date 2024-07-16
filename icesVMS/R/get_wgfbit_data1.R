#' Download swept area ratio, landings and value map data
#'
#' Download a data.frame of surface and subsurface swept area
#' ratio by c-square for a given ICES ecoregion, year and gear code.
#'
#' @param ecoregion ICES ecoregion
#' @param year which year to select
#' @param fishing_category optional gear category ("Otter", "Dredge")
#' @param benthis_metier optional benthis metier ("SDN_DMF")
#' @param datacall integer year giving which data call year to inquire about.
#'   If NULL returns the a summary of the most recent approved data.
#' @param convert2sf logical, default FALSE, should an simple features object
#'   be returned if the \code{sf} package is installed?
#'
#' @return a data.frame with a WKT column for the c-square polygons
#'
#' @details
#'
#' fishing_category and benthis_metier may not both be supplied, if neither
#' are supplied the total is calculated.
#'
#' @examples 
#' \dontrun{
#' # requires authorization
#' data1 <- get_wgfbit_data1("Celtic Seas", 2021, benthis_metier = "OT_DMF", convert2sf = TRUE)
#' plot(data1["total_weight"], border = NA, logz = TRUE)
#' }
#' 
#' @export
get_wgfbit_data1 <- function(ecoregion, year, fishing_category = NULL, benthis_metier = NULL, 
  datacall = NULL, convert2sf = FALSE) {
  
  check_ecoregion(ecoregion)

  if (!is.null(fishing_category)) {
    # warn if both gear_group and benthis_metier are supplied
    if (!is.null(benthis_metier)) {
      warning("Both fishing_category and benthis_metier were supplied, only fishing_category was used.")
    }

    benthis_metier = NULL
  }

  url <-
    vms_api(
      glue("wgfbit/dataset1/{URLencode(ecoregion)}/{year}"),
      fishing_category = fishing_category, benthis_metier = benthis_metier, 
      datacall = datacall
    )

  out <- vms_get(url, use_token = TRUE)

  if (convert2sf) {
    convert_df2sf(out)
  } else {
    out
  }
}
