#' Download fishing effort summaries
#'
#' Download a data.frame of kw fishing hours by country and year for a
#' given ICES ecoregion.
#'
#' @param ecoregion ICES ecoregion
#'
#' @return a data.frame
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' ns_effort_data <- get_fo_effort("Greater North Sea")
#' }
#' 
#' @export
get_fo_effort <- function(ecoregion) {
  check_ecoregion(ecoregion)
  
  url <- 
    vms_api(
      glue("fisheriesoverviews/effortmap/{URLencode(ecoregion)}")
    )

  vms_get(url, use_token = TRUE)
}
