#' Forms breeding group(s) with an effort to match a specified sex ratio
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @description The sex ratio is the ratio of females to males.
#'
#' @return A list containing one character vector of animal IDs such that the
#'         sex ratio of the group is as close as possible to the ratio
#'         specified by \code{sexRatio}.
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param groupMembers list initialized and ready to receive groups with the
#' desired sex ratios that are created within this function
#' @param grpNum is a list \code{numGp} long with each member an integer
#' vector of \code{1:numGp}.
#' @param kin list of animals and those animals who are related above a
#' threshold value.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @param sexRatio numeric value indicating the ratio of females to males x
#' from 0.5 to 20 by increments of 0.5.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' examplePedigree <- nprcgenekeepr::examplePedigree
#' examplePedigree <- examplePedigree[1:300, ] # Comment out for full example
#' ped <- qcStudbook(examplePedigree,
#'   minParentAge = 2L, reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#'
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
#' currentGroups <- list(1)
#' currentGroups[[1]] <- examplePedigree$id[1L:3L]
#' candidates <- examplePedigree$id[examplePedigree$status == "ALIVE"]
#' threshold <- 0.015625
#' kin <- getAnimalsWithHighKinship(kmat, ped, threshold, currentGroups,
#'   ignore = list(c("F", "F")), minAge = 1L
#' )
#' # Filtering out candidates related to current group members
#' conflicts <- unique(c(
#'   unlist(kin[unlist(currentGroups)]),
#'   unlist(currentGroups)
#' ))
#' candidates <- setdiff(candidates, conflicts)
#'
#' kin <- addAnimalsWithNoRelative(kin, candidates)
#'
#' ignore <- NULL
#' minAge <- 1.0
#' numGp <- 1L
#' harem <- FALSE
#' sexRatio <- 0.0
#' withKin <- FALSE
#' groupMembers <- nprcgenekeepr::makeGroupMembers(numGp,
#'   currentGroups,
#'   candidates,
#'   ped,
#'   harem = harem,
#'   minAge = minAge
#' )
#' groupMembersStart <- groupMembers
#' grpNum <- nprcgenekeepr::makeGrpNum(numGp)
#'
#' groupMembers <- fillGroupMembersWithSexRatio(
#'   candidates, groupMembers, grpNum, kin, ped, minAge, numGp,
#'   sexRatio = 1.0
#' )
fillGroupMembersWithSexRatio <-
  function(candidates, groupMembers, grpNum, kin, ped, minAge, numGp,
           sexRatio) {
    potentialSires <- getPotentialSires(candidates, ped, minAge)
    availableMales <- makeAvailable(potentialSires, numGp)
    availableFemales <- makeAvailable(setdiff(candidates, potentialSires),
                                      numGp)

    repeat {
      if (isEmpty(grpNum)) {
        break
      }

      # Select a group at random
      i <- sample(grpNum, 1L)[[1L]]

      # Select an animal that can be added to this group and add it
      ratio <- calculateSexRatio(groupMembers[[i]], ped)
      if (is.na(ratio)) {
        ratio <- 0.0
      } ## no seed animals

      if (ratio < sexRatio) { ## need female
        id <- sample(availableFemales[[i]], 1L)
        availableFemales <-
          removeSelectedAnimalFromAvailableAnimals(availableFemales, id, numGp)
      } else { # may need male # nolint unnecessary_nesting_linter
        if (abs(sexRatio - calculateSexRatio(groupMembers[[i]], ped,
          additionalMales = 1L
        )) <
          abs(sexRatio - calculateSexRatio(groupMembers[[i]], ped,
            additionalFemales = 1L
          ))) {
          id <- sample(availableMales[[i]], 1L)
          availableMales <-
            removeSelectedAnimalFromAvailableAnimals(availableMales, id, numGp)
        } else {
          id <- sample(availableFemales[[i]], 1L)
          availableFemales <-
            removeSelectedAnimalFromAvailableAnimals(availableFemales, id,
                                                     numGp)
        }
      }
      groupMembers[[i]] <- c(groupMembers[[i]], id)
      # Remove all relatives from consideration for the group it was added to
      availableMales[[i]] <- setdiff(availableMales[[i]], kin[[id]])
      availableFemales[[i]] <- setdiff(availableFemales[[i]], kin[[id]])
      grpNum <- removeGroupIfNoAvailableAnimals(grpNum, availableMales)
      grpNum <- removeGroupIfNoAvailableAnimals(grpNum, availableFemales)
    }
    groupMembers
  }
