#' addIdRecords Adds Ego records added having NAs for parent IDs
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Pedigree with Ego records added having NAs for parent IDs
#'
#' @examples
#' uPedOne <- data.frame(
#'   id = c("d1", "s2", "d2", "o1", "o2", "o3", "o4"),
#'   sire = c("s0", "s4", NA, "s1", "s1", "s2", "s2"),
#'   dam = c("d0", "d4", NA, "d1", "d2", "d2", "d2"),
#'   sex = c("F", "M", "F", "F", "F", "F", "M"),
#'   stringsAsFactors = FALSE
#' )
#' pedOne <- data.frame(
#'   id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
#'   sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
#'   dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
#'   sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
#'   stringsAsFactors = FALSE
#' )
#' pedOne[!pedOne$id %in% uPedOne$id, ]
#' newPed <- addIdRecords(ids = "s1", pedOne, uPedOne)
#' pedOne[!pedOne$id %in% newPed$id, ]
#' newPed[newPed$id == "s1", ]
#'
#' @param ids character vector of IDs to be added as Ego records having
#' NAs for parent IDs
#' @param fullPed a trimmed pedigree
#' @param partialPed a trimmed pedigree dataframe with uninformative founders
#' removed.
#' @importFrom data.table rbindlist
#' @export
addIdRecords <- function(ids, fullPed, partialPed) {
  if (length(ids[!all(is.na(ids))]) > 0L) {
    addToPed <- fullPed[is.element(fullPed$id, ids), ]
    addToPed$sire <- NA
    addToPed$dam <- NA
    partialPed <- rbindlist(list(partialPed, addToPed))
  }
  partialPed[!duplicated(partialPed$id), ]
}
