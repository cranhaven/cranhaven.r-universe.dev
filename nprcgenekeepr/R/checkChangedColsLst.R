#' checkChangedColsLst examines list for non-empty fields
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Returns \code{NULL} if all fields are empty
#' else the entire list is returned.
#'
#' @param changedCols list with fields for each type of column change
#' \code{qcStudbook}.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' library(lubridate)
#' pedOne <- data.frame(
#'   ego_id = c(
#'     "s1", "d1", "s2", "d2", "o1", "o2", "o3",
#'     "o4"
#'   ),
#'   `si re` = c(NA, NA, NA, NA, "s1", "s1", "s2", "s2"),
#'   dam_id = c(NA, NA, NA, NA, "d1", "d2", "d2", "d2"),
#'   sex = c("F", "M", "M", "F", "F", "F", "F", "M"),
#'   birth_date = mdy(
#'     paste0(
#'       sample(1:12, 8, replace = TRUE), "-",
#'       sample(1:28, 8, replace = TRUE), "-",
#'       sample(seq(0, 15, by = 3), 8, replace = TRUE) +
#'         2000
#'     )
#'   ),
#'   stringsAsFactors = FALSE, check.names = FALSE
#' )
#'
#' errorLst <- qcStudbook(pedOne, reportErrors = TRUE, reportChanges = TRUE)
#' checkChangedColsLst(errorLst$changedCols)
checkChangedColsLst <- function(changedCols) {
  if (length(changedCols$caseChange) > 0L ||
    length(changedCols$spaceRemoved) > 0L ||
    length(changedCols$periodRemoved) > 0L ||
    length(changedCols$underScoreRemoved) > 0L ||
    length(changedCols$egoToId) > 0L ||
    length(changedCols$egoidToId) > 0L ||
    length(changedCols$sireIdToSire) > 0L ||
    length(changedCols$damIdToDam) > 0L ||
    length(changedCols$birthdateToBirth) > 0L ||
    length(changedCols$deathdateToDeath) > 0L ||
    length(changedCols$recordstatusToRecordStatus) > 0L ||
    length(changedCols$fromcenterToFromCenter) > 0L) {
    TRUE
  } else {
    FALSE
  }
}
