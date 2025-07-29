#' Creates a empty errorLst object
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr

#' @return An errorLst object with placeholders for error types found in a
#' pedigree file by \code{qcStudbook}.
#'
#' @export
#' @examples
#' library(nprcgenekeepr)
#' getEmptyErrorLst()
getEmptyErrorLst <- function() {
  emptyErrorLst <- list(
    failedDatabaseConnection = character(0L),
    missingColumns = character(0L),
    invalidDateRows = character(0L),
    suspiciousParents = data.frame(),
    femaleSires = character(0L),
    maleDams = character(0L),
    sireAndDam = character(0L),
    duplicateIds = character(0L),
    changedCols = list(
      caseChange = character(0L),
      spaceRemoved = character(0L),
      periodRemoved = character(0L),
      underScoreRemoved = character(0L),
      egoToId = character(0L),
      egoidToId = character(0L),
      sireIdToSire = character(0L),
      damIdToDam = character(0L),
      birthdateToBirth = character(0L),
      deathdateToDeath = character(0L),
      recordstatusToRecordStatus = character(0L),
      fromcenterToFromCenter = character(0L),
      geographicoriginToGeographicOrigin = character(0L)
    )
  )
  class(emptyErrorLst) <-
    append(class(emptyErrorLst), "nprcgenekeeprErr")
  emptyErrorLst
}
