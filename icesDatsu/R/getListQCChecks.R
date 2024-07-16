#' get list of datasets
#'
#' This API allows the user to have a list of all the datasets IDs that can be screened in the DATSU API.
#'
#' @param datasetverID the dataset ID
#' @param recordType string name of the record type, optional
#'
#' @return The list of Datasets that can be screened in DATSU with the IDs
#'
#' @examples
#' \donttest{
#' getListQCChecks(145)
#' getListQCChecks(145, "VE")
#' }
#' @export
getListQCChecks <- function(datasetverID, recordType = NULL) {
  if (is.null(recordType)) {
    url <-
      datsu_api(
        paste0("getListQCChecks/", datasetverID)
      )
  } else {
    url <-
      datsu_api(
        paste0("getListQCChecks/", datasetverID),
        RecordType = recordType
      )
  }

  # perform request
  out <- datsu_get(url)

  # some formatting and mods
  out$sqlText <- tolower(out$sqlText)
  out$sqlText <- gsub("year[(]getdate[(][)][)]", 2022, out$sqlText)
  message("year hard wired to 2022 for now")
  out$sqlText <- gsub("len[(]", "length(", out$sqlText)
  #out$sqlText <- gsub("isnull[(]", "ifnull(", out$sqlText)

  out
}
