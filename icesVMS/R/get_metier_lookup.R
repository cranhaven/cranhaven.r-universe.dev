#' Link Metier level 6 to Benthis categories
#'
#' Download a data.frame of Metier codes to link
#' level 6 metier codes with different gear categories
#'
#' @return a data.frame
#' 
#' @examples
#' \donttest{
#' metier_lookup <- get_metier_lookup()
#' }
#'
#' @export
get_metier_lookup <- function() {
  vms_get(
    vms_api("MetierLookup")
  )
}
