#' hasBothParents checks to see if both parents are identified.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return TRUE if ID has both sire and dam identified in \code{ped}.
#'
#' @param id character vector of IDs to examine for parents
#' @param ped a pedigree
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::pedOne
#' names(ped) <- c("id", "sire", "dam", "sex", "birth")
#' hasBothParents("o2", ped)
#' ped$sire[ped$id == "o2"] <- NA
#' hasBothParents("o2", ped)
hasBothParents <- function(id, ped) {
  !is.na(ped$sire[ped$id == id]) & !is.na(ped$dam[ped$id == id])
}
