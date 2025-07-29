#' checkErrorLst examines list for non-empty fields
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Returns FALSE if all fields are empty or the list is NULL
#' else the entire list is returned.
#'
#' @param errorLst list with fields for each type of error detectable by
#' \code{qcStudbook}.
#' @export
#' @examples
#' errorLst <- qcStudbook(nprcgenekeepr::pedFemaleSireMaleDam,
#'   reportErrors = TRUE
#' )
#' checkErrorLst(errorLst)
checkErrorLst <- function(errorLst) {
  if (is.null(errorLst)) {
    return(FALSE)
  }
  if (length(errorLst$failedDatabaseConnection) > 0L ||
    length(errorLst$missingColumns) > 0L ||
    length(errorLst$invalidDateRows) > 0L ||
    length(errorLst$sireAndDam) > 0L ||
    length(errorLst$femaleSires) > 0L ||
    length(errorLst$maleDams) > 0L ||
    length(errorLst$duplicateIds) > 0L ||
    nrow(errorLst$suspiciousParents) > 0L) {
    TRUE
  } else {
    FALSE
  }
}
