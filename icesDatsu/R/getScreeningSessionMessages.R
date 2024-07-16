#' get screening session messages
#'
#' A list of the messages for the specified session ID
#'
#' @param sessionID the session ID
#'
#' @return List of the messages for the specified session
#'
#' @examples
#' \donttest{
#' messages <- getScreeningSessionMessages(10)
#' }
#' @export
getScreeningSessionMessages <- function(sessionID) {
  url <- datsu_api(
    paste0("getScreeningSessionMessages/", sessionID)
  )

  out <- datsu_get(url)

  out
}
