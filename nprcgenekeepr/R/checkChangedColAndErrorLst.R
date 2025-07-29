#' checkChangedColAndErrorLst examines errorLst for errors and
#' errorLst$changeCols non-empty fields
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Returns \code{NULL} is all fields are empty
#' else the entire list is returned.
#'
#' @param errorLst list with fields for each type of changed column and
#' error detectable by \code{qcStudbook}.
#' @noRd
checkChangedColAndErrorLst <- function(errorLst) {
  if (checkErrorLst(errorLst) ||
    checkChangedColsLst(errorLst$changedCols)) {
    errorLst
  } else {
    NULL
  }
}
