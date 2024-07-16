#' get screening session list
#'
#' This API allows the user to screen a file using the API
#'
#' @param dataSetVerID the dataset version id
#' @param year filter by year, optional
#' @param country filter by country, optional
#'
#' @return The details of the sessions
#'
#' @examples
#' \donttest{
#' getScreeningSessionsList(145)
#' getScreeningSessionsList(145, year = 2020)
#' }
#' @export
getScreeningSessionsList <- function(dataSetVerID, year = NULL, country = NULL) {
  url <- datsu_api(
    paste0("getScreeningSessionsList/", dataSetVerID),
    year = year,
    country = country
  )

  out <- datsu_get(url)

  out
}
