#' getIdsWithOneParent extracts IDs of animals pedigree without either a
#' sire or a dam
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Character vector of all single parents
#'
#' @param uPed a trimmed pedigree dataframe with uninformative founders removed.
#' @export
#' @examples
#' examplePedigree <- nprcgenekeepr::examplePedigree
#' breederPed <- qcStudbook(examplePedigree,
#'   minParentAge = 2,
#'   reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#' probands <- breederPed$id[!(is.na(breederPed$sire) &
#'   is.na(breederPed$dam)) &
#'   is.na(breederPed$exit)]
#' ped <- getProbandPedigree(probands, breederPed)
#' nrow(ped)
#' p <- removeUninformativeFounders(ped)
#' nrow(p)
#' p <- addBackSecondParents(p, ped)
#' nrow(p)
getIdsWithOneParent <- function(uPed) {
  uPed$id[(is.na(uPed$sire) & !is.na(uPed$dam)) |
    (!is.na(uPed$sire) & is.na(uPed$dam))]
}
