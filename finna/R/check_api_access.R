#' @title Check Access to the Finna API
#'
#' @description
#' This function tests whether R can successfully connect to the Finna API by downloading
#' the OpenAPI specification from `https://api.finna.fi/api/v1/?openapi`. It returns
#' a logical value indicating the accessibility of the API.
#'
#' @return
#' A logical value:
#' - `TRUE`: The API is accessible.
#' - `FALSE`: The API is not accessible.
#'
#' @importFrom httr status_code
#' @importFrom curl curl_download
#' @export
#' @md
#' @examples
#' \dontrun{
#'   # Check if the API is accessible
#'   access <- check_api_access()
#'   if (access) {
#'     message("Finna API is accessible")
#'   } else {
#'     message("Finna API is not accessible")
#'   }
#' }

check_api_access <- function() {
  temp <- tempfile()
  http_url <- "https://api.finna.fi/api/v1/?openapi"

  suppressWarnings(
    try(
      curl::curl_download(http_url, temp, quiet = TRUE),
      silent = TRUE
    )
  )

  if (is.na(file.info(temp)$size)) {
    FALSE
  } else {
    TRUE
  }
}
