#' Download fishing landings summaries
#'
#' Download a data.frame of total weight by country and year for a
#' given ICES ecoregion.
#'
#' @param ecoregion ICES ecoregion
#'
#' @return a data.frame
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' ns_landings_data <- get_fo_landings("Greater North Sea")
#' }
#' 
#' @export
get_fo_landings <- function(ecoregion) {
  check_ecoregion(ecoregion)

  url <-
    vms_api(
      glue("fisheriesoverviews/landings/{URLencode(ecoregion)}")
    )

  vms_get(url, use_token = TRUE)
}
