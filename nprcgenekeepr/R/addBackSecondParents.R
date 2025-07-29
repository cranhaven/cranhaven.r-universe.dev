#' Add back single parents trimmed pedigree
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Uses the \code{ped} dataframe, which has full complement of parents and the
#' \code{uPed} dataframe, which has all uninformative parents removed to
#' add back single parents to the \code{uPed} dataframe where one parent is
#' known. The parents are added back to the pedigree as an ID record with
#' NA for both sire and dam of the added back ID.
#'
#' @return A dataframe with pedigree with single parents added.
#'
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
#'
#' @param uPed a trimmed pedigree dataframe with uninformative founders removed.
#' @param ped a trimmed pedigree
#' @export
addBackSecondParents <- function(uPed, ped) {
  # Adding back second parents where one is known
  idsWithOneParent <- getIdsWithOneParent(uPed)
  addBack <- character(0L)
  ## Within this loop, the second parent is added to uPed as the sire
  ## or dam as is found in ped.
  for (id in idsWithOneParent) {
    if (hasBothParents(id, ped)) {
      if (is.na(uPed$sire[uPed$id == id])) {
        addBack <- c(addBack, ped$sire[ped$id == id])
        uPed[(uPed$id == id), "sire"] <- ped$sire[ped$id == id]
      } else {
        addBack <- c(addBack, ped$dam[ped$id == id])
        uPed[(uPed$id == id), "dam"] <- ped$dam[ped$id == id]
      }
    }
  }
  addIdRecords(ids = addBack, fullPed = ped, partialPed = uPed)
}
