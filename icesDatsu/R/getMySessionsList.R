#' get screening session list for a user
#'
#' This API of the web service returns a list of the sessions for the authenticated user.
#'
#' @return The details of the users sessions
#'
#' @examples
#'
#' \dontrun{
#' getMySessionsList()
#' }
#' @export
getMySessionsList <- function() {
  url <- datsu_api("getMySessionsList")

  out <- datsu_get(url, use_token = TRUE)

  out
}
