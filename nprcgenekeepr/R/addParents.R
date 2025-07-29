#' Add parents
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Pedigree curation function
#' Given a pedigree, find any IDs listed in the "sire" or "dam" columns
#' that lack their own line entry and generate one.
#'
#' This must be run after to \code{addUIds} since the IDs made there are
#' used by \code{addParents}
#'
#' @return An updated pedigree with entries added as necessary.
#' Entries have the id and sex specified; all remaining columns are filled
#' with \code{NA}.
#'
#' @examples
#' pedTwo <- data.frame(
#'   id = c("d1", "s2", "d2", "o1", "o2", "o3", "o4"),
#'   sire = c(NA, NA, NA, "s1", "s1", "s2", "s2"),
#'   dam = c(NA, NA, NA, "d1", "d2", "d2", "d2"),
#'   sex = c("F", "M", "F", "F", "F", "F", "M"),
#'   stringsAsFactors = FALSE
#' )
#' newPed <- addParents(pedTwo)
#' newPed
#'
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @export
addParents <- function(ped) {
  sires <- ped$sire
  dams <- ped$dam

  # Finding sires and dams not in the id column
  a1 <- sires[!(sires %in% ped$id) & !is.na(sires)]
  a1 <- a1[!duplicated(a1)]
  a2 <- dams[!(dams %in% ped$id) & !is.na(dams)]
  a2 <- a2[!duplicated(a2)]

  a1 <- data.frame(id = a1, stringsAsFactors = FALSE)
  a2 <- data.frame(id = a2, stringsAsFactors = FALSE)

  ped <- ped[, names(ped) != "recordStatus"]
  ped <- cbind(ped, recordStatus = "original", stringsAsFactors = FALSE)

  # Adding line entries for these parents
  if (nrow(a1) > 0L) {
    a1$sire <- NA
    a1$dam <- NA
    a1$sex <- "M"
    a1$recordStatus <- "added"
    ped <- rbindFill(ped, a1)
  }

  if (nrow(a2) > 0L) {
    a2$sire <- NA
    a2$dam <- NA
    a2$sex <- "F"
    a2$recordStatus <- "added"
    ped <- rbindFill(ped, a2)
  }
  ped
}
