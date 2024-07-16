#' get screening session details
#'
#' This API allows the user to view the details of a specific session.
#'
#' @param sessionID the session ID
#'
#' @return Details of the session
#'
#' @examples
#' \donttest{
#' getScreeningSessionDetails(10)
#' }
#' @export
getScreeningSessionDetails <- function(sessionID) {
  url <- datsu_api(
    paste0("getScreeningSessionDetails/", sessionID)
  )

  out <- datsu_get(url)

  out
}
