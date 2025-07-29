#' Add animals to an existing breeding group or forms groups:
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Group Formation
#'
#' @description{
#' \code{groupAddAssign} finds the largest group that can be formed by adding
#' unrelated animals from a set of candidate IDs to an existing group, to a new
#' group it has formed from a set of candidate IDs or if more than 1 group
#' is desired, it finds the set of groups with the largest average size.
#'
#' The function implements a maximal independent set (MIS) algorithm to find
#' groups of unrelated animals. A set of animals may have many different MISs of
#' varying sizes, and finding the largest would require traversing all possible
#' combinations of animals. Since this could be very time consuming, this
#' algorithm produces a random sample of the possible MISs, and selects from
#' these. The size of the random sample is determined by the specified number
#' of iterations.
#' }
#'
#' @return A list with list items \code{group}, \code{score} and optionally
#' \code{groupKin}.
#' The list item \code{group} contains a list of the best group(s) produced
#' during the simulation.
#' The list item \code{score} provides the score associated with the group(s).
#' The list item \code{groupKin} contains the subset of the kinship matrix
#' that is specific for each group formed.
#'
#' @param candidates Character vector of IDs of the animals available for
#' use in forming the groups. The animals that may be present in
#' \code{currentGroups} are not included within \code{candidates}.
#' @param currentGroups List of character vectors of IDs of animals currently
#' assigned to groups.
#' Defaults to a list with character(0) in each sublist element (one for each
#' group being formed) assuming no groups are prepopulated.
#' @param kmat Numeric matrix of pairwise kinship values. Rows and columns
#' are named with animal IDs.
#' @param ped Dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param threshold Numeric value indicating the minimum kinship level to be
#' considered in group formation. Pairwise kinship below this level will be
#' ignored. The default value is 0.015625.
#' @param ignore List of character vectors representing the sex combinations
#' to be ignored. If provided, the vectors in the list specify if pairwise
#' kinship should be ignored between certain sexes.
#' Default is to ignore all pairwise kinship between females.
#' @param minAge Integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param iter Integer indicating the number of times to perform the random
#' group formation process. Default value is 1000 iterations.
#' @param numGp Integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @param harem Logical variable when set to \code{TRUE}, the formed groups
#' have a single male at least \code{minAge} old.
#' @param sexRatio Numeric value indicating the ratio of females to males x
#' from 0.5 to 20 by increments of 0.5.
#' @param withKin Logical variable when set to \code{TRUE}, the kinship
#' matrix for the group is returned along with the group and score.
#' Defaults to not return the kinship matrix. This maintains compatibility with
#' earlier versions.
#' @param updateProgress Function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#'
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
#' ped <- trimPedigree(probands, ped,
#'   removeUninformative = FALSE,
#'   addBackParents = FALSE
#' )
#' geneticValue <- reportGV(ped,
#'   guIter = 50, # should be >= 1000
#'   guThresh = 3,
#'   byID = TRUE,
#'   updateProgress = NULL
#' )
#' trimmedGeneticValue <- reportGV(trimmedPed,
#'   guIter = 50, # should be >= 1000
#'   guThresh = 3,
#'   byID = TRUE,
#'   updateProgress = NULL
#' )
#' candidates <- trimmedPed$id[trimmedPed$birth < as.Date("2013-01-01") &
#'   !is.na(trimmedPed$birth) &
#'   is.na(trimmedPed$exit)]
#' haremGrp <- groupAddAssign(
#'   kmat = trimmedGeneticValue[["kinship"]],
#'   ped = trimmedPed,
#'   candidates = candidates,
#'   iter = 10, # should be >= 1000
#'   numGp = 6,
#'   harem = TRUE
#' )
#' haremGrp$group
#' sexRatioGrp <- groupAddAssign(
#'   kmat = trimmedGeneticValue[["kinship"]],
#'   ped = trimmedPed,
#'   candidates = candidates,
#'   iter = 10L, # should be >= 1000L
#'   numGp = 6L,
#'   sexRatio = 9.0
#' )
#' sexRatioGrp$group
groupAddAssign <- function(candidates,
                           kmat,
                           ped,
                           currentGroups = list(character(0L)),
                           threshold = 0.015625, ignore = list(c("F", "F")),
                           minAge = 1.0, iter = 1000L,
                           numGp = 1L, harem = FALSE,
                           sexRatio = 0.0, withKin = FALSE,
                           updateProgress = NULL) {
  if (length(currentGroups) > numGp) {
    stop(
      "Cannot have more groups with seed animals than number of ",
      "groups to be formed."
    )
  }
  kmat <- filterKinMatrix(union(candidates, unlist(currentGroups)), kmat)
  kin <- getAnimalsWithHighKinship(
    kmat, ped, threshold, currentGroups, ignore,
    minAge
  )

  # Filtering out candidates related to current group members
  conflicts <- unique(c(
    unlist(kin[unlist(currentGroups)]),
    unlist(currentGroups)
  ))
  candidates <- setdiff(candidates, conflicts)


  kin <- addAnimalsWithNoRelative(kin, candidates)
  if (harem &&
      length(getPotentialSires(candidates, ped, minAge)) < numGp) {
    stop(
      "User selected to form ",
      numGp,
      " harems with only ",
      length(getPotentialSires(candidates, ped, minAge)),
      " males at least ",
      minAge,
      " years old in the list of candidates."
    )
  }

  # Starting the group assignment simulation
  savedScore <- -1L
  savedGroupMembers <- list()

  for (k in 1L:iter) {
    groupMembers <- fillGroupMembers(
      candidates, currentGroups, kin, ped, harem,
      minAge, numGp, sexRatio
    )

    # Score the resulting groups
    score <- min(lengths(groupMembers))

    if (score > savedScore) {
      savedGroupMembers <- groupMembers
      savedScore <- score
    }

    # Updating the progress bar, if applicable
    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }

  savedGroupMembers <- addGroupOfUnusedAnimals(
    savedGroupMembers, candidates,
    ped, minAge, harem
  )

  groupMembersReturn(savedGroupMembers, savedScore, withKin, kmat)
}
