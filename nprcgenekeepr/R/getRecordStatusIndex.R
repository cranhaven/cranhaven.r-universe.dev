#' Returns record numbers with selected \code{recordStatus}.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return An integer vector of records with \code{recordStatus} ==
#' \code{status}.
#'
#' @param ped pedigree dataframe
#' @param status character vector with value of \code{"added"} or
#' \code{"original"}.
#' @noRd
getRecordStatusIndex <- function(ped, status = "added") {
  if (any("recordStatus" %in% names(ped))) {
    seq_along(ped$recordStatus)[ped$recordStatus == status]
  } else {
    integer(0L)
  }
}
