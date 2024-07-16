#' get list of records for a dataset
#'
#' A list of the available records for the specified format
#'
#' @param datasetverID the dataset ID
#'
#' @return List of the supported records for the specified records
#'
#' @examples
#' \donttest{
#' getRecordIDs(145)
#' }
#' @export
getRecordIDs <- function(datasetverID) {
  url <- datsu_api(
    paste0("getRecordIDs/", datasetverID)
  )
  out <- datsu_get(url)

  out
}