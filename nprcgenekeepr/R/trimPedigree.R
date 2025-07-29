#' Trim pedigree to ancestors of provided group by removing uninformative
#' individuals
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Filters a pedigree down to only the ancestors of the provided group,
#' removing unnecessary individuals from the studbook. This version builds
#' the pedigree back in time starting from a group of probands, then moves
#' back down the tree trimming off uninformative ancestors.
#'
#' @return A pedigree that has been trimmed, had uninformative founders
#' removed and single parents added back.
#'
#' @param probands a character vector with the list of animals whose ancestors
#' should be included in the final pedigree.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The fields \code{sire} and \code{dam} are required.
#' @param removeUninformative logical defaults to \code{FALSE}. If set to
#' \code{TRUE}, uninformative founders are removed.
#'
#' Founders (having unknown sire and dam) that appear only one time in a
#' pedigree are uninformative and can be removed from a pedigree without loss
#' of information.
#' @param addBackParents logical defaults to \code{FALSE}. If set to
#' \code{TRUE}, the function adds back single parents to the \code{p} dataframe
#' when one parent is known.
#' The function \code{addBackSecondParents} uses the \code{ped} dataframe,
#' which has full complement of parents and the
#' \code{p} dataframe, which has all uninformative parents removed to add
#' back single parents to the \code{p} dataframe.
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
#' breederPed <- setPopulation(ped = breederPed, ids = focalAnimals)
#' trimmedPed <- trimPedigree(focalAnimals, breederPed)
#' trimmedPedInformative <- trimPedigree(focalAnimals, breederPed,
#'   removeUninformative = TRUE
#' )
#' nrow(breederPed)
#' nrow(trimmedPed)
#' nrow(trimmedPedInformative)
trimPedigree <- function(probands, ped, removeUninformative = FALSE,
                         addBackParents = FALSE) {
  ped <- getProbandPedigree(probands, ped)
  if (removeUninformative) {
    p <- removeUninformativeFounders(ped)
    if (addBackParents) {
      p <- addBackSecondParents(p, ped)
    }
  } else {
    p <- ped
  }
  return(p)
}
