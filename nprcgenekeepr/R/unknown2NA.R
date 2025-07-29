#' Removing IDs having "UNKNOWN" regardless of case
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Someone started entering "unknown" for unknown parents instead of leaving
#' the field blank in PRIMe.
#' @return A dataframe with "UNKNOWN" values in the columns \code{id},
#' \code{sire}, and \code{dam} replaced with NA
#' @param ped  A dataframe containing at least and "id" field
#' @noRd
unknown2NA <- function(ped) {
  if ("id" %in% names(ped)) {
    ped <- ped[toupper(ped$id) != "UNKNOWN", ]
  }
  if ("sire" %in% names(ped)) {
    ped$sire[toupper(ped$sire) == "UNKNOWN"] <- NA
  }
  if ("dam" %in% names(ped)) {
    ped$dam[toupper(ped$dam) == "UNKNOWN"] <- NA
  }
  ped
}
