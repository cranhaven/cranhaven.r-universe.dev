#' Get all messages of a file screening session
#'
#' Download a list of error and warning messages on a file upload screening session.
#'
#' @param sessionId file screening session ID
#'
#' @return a list
#'
#' @examples
#' \donttest{
#' tofix <- get_screening_messages(528)
#' }
#' @export
get_screening_messages <- function(sessionId) {
  vms_get(
    glue("https://data.ices.dk/vms/webapi/getScreeningSessionMessages/{sessionId}")
  )
}
