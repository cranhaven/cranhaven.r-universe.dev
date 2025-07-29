#' removeUnknownAnimals Removes unknown animals added to pedigree that serve as
#' placeholders for unknown parents.
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Pedigree with unknown animals removed
#'
#' @param ped pedigree dataframe
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::smallPed
#' addedPed <- cbind(ped,
#'   recordStatus = rep("original", nrow(ped)),
#'   stringsAsFactors = FALSE
#' )
#' addedPed[1:3, "recordStatus"] <- "added"
#' ped2 <- removeUnknownAnimals(addedPed)
#' nrow(ped)
#' nrow(ped2)
removeUnknownAnimals <- function(ped) {
  ped[getRecordStatusIndex(ped, status = "original"), ]
}
