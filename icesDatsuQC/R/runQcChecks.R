#' Runs quality checks on a dataset
#'
#' Check a data set against the ICES DATSU data submission utility,
#' the user must supply a dataset version and record type.
#'
#' @param filename the filename of the file to check
#' @param datasetverID the dataset ID
#' @param recordType string name of the record type
#'
#' @return The list of Datasets that can be screened in DATSU with the IDs
#'
#' @examples
#' \donttest{
#' filename <- system.file("test_files/vms_test.csv", package = "icesDatsuQC")
#' runQCChecks(filename, 145, "VE")
#' }
#' @export
#' @importFrom sqldf sqldf
#' @importFrom utils read.csv
#' @importFrom icesDatsu getListQCChecks getDataFieldsDescription
runQCChecks <- function(filename, datasetverID, recordType) {
  qc <- getListQCChecks(datasetverID, recordType)

  data <- read.csv(filename, header = FALSE)
  names(data) <- getDataFieldsDescription(datasetverID, recordType)$fieldcode
  tablename <- paste0("R", recordType)
  assign(tablename, data)

  checks <-
    lapply(
      1:nrow(qc),
      function(i) {
        try(
          sqldf(
            paste(
              "select * from", tablename, "where not ",
              qc$sqlText[i]
            )
          )
        )
      }
    )
  names(checks) <- qc$check_Description

  fails <- checks[which(sapply(checks, function(x) !inherits(x, "try-error") && nrow(x) > 0))]
  if (length(fails) > 0) {
    fails
  } else {
    message("all checks possible to run in R passed")
    invisible(NULL)
  }
}
