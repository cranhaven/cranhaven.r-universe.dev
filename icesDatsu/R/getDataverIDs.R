#' get list of datasets
#'
#' This API allows the user to have a list of all the datasets IDs that can be screened in the DATSU API.
#'
#' @return The list of Datasets that can be screened in DATSU with the IDs
#'
#' @examples
#' \donttest{
#' formats <- getDataverIDs()
#' formats[grep("vms", tolower(formats$description)), ]
#'}
#' @export
getDataverIDs <- function() {
  url <- datsu_api("getDataverIDs")

  out <- datsu_get(url)

  out
}
