#' get list of datasets
#'
#' A list of the available fields for the specified format and record (optional filter).
#' in this list it is specified the field type, if it is mandatory and if it is linked with any
#' vocabulary.
#'
#' @param datasetverID the dataset ID
#' @param recordType string name of the record type, optional
#'
#' @return List of the supported records and fields.
#'
#' @examples
#'
#' \donttest{
#' getDataFieldsDescription(145)
#' getDataFieldsDescription(145, "VE")
#' }
#' @export
getDataFieldsDescription <- function(datasetverID, recordType = NULL) {
  if (is.null(recordType)) {
    url <-
      datsu_api(
        paste0("getDataFieldsDescription/", datasetverID)
      )
  } else {
    url <-
      datsu_api(
        paste0("getDataFieldsDescription/", datasetverID),
        RecordType = recordType
      )
  }

  # perform request
  out <- datsu_get(url)

  out
}
