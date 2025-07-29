#' Finds the number of total offspring for each animal in the provided pedigree.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Genetic Value Analysis
#'
#' @return A named vector containing the offspring counts for each animal in
#' \code{probands}. Rownames are set to the IDs from \code{probands}.
#'
#' @param probands character vector of egos for which offspring should be
#' counted and returned.
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#' This requires complete pedigree information.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' examplePedigree <- nprcgenekeepr::examplePedigree
#' breederPed <- qcStudbook(examplePedigree,
#'   minParentAge = 2,
#'   reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#' focalAnimals <- breederPed$id[!(is.na(breederPed$sire) &
#'   is.na(breederPed$dam)) &
#'   is.na(breederPed$exit)]
#' ped <- setPopulation(ped = breederPed, ids = focalAnimals)
#' trimmedPed <- trimPedigree(focalAnimals, breederPed)
#' probands <- ped$id[ped$population]
#' totalOffspring <- findOffspring(probands, ped)
findOffspring <- function(probands, ped) {
  sires <- tapply(ped$id, as.factor(ped$sire), length)
  dams <- tapply(ped$id, as.factor(ped$dam), length)
  offspring <- c(sires, dams)

  idx <- match(probands, names(offspring))
  offspring <- offspring[idx]
  names(offspring)[is.na(idx)] <- probands[is.na(idx)]
  offspring[is.na(idx)] <- 0L

  offspring
}
