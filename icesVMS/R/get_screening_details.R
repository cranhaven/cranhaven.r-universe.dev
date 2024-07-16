#' Get the details of a file screening session
#'
#' Download a list of information on a file upload screening session.
#'
#' @param sessionId file screening session ID
#'
#' @return a list
#' 
#' @examples
#' \donttest{
#' screening_info <- get_screening_details(530)
#' }
#' @export
get_screening_details <- function(sessionId) {
  vms_get(
    glue("https://data.ices.dk/vms/webapi/getScreeningDetails/{sessionId}")
  )
}
