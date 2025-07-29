#' Forms and fills list of animals groups based on provided constraints
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A list of animal groups and their member animals
#'
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param currentGroups list of character vectors of IDs of animals currently
#' assigned
#' to the group. Defaults to character(0) assuming no groups are existent.
#' @param kin list of animals and those animals who are related above a
#' threshold value.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param harem logical variable when set to \code{TRUE}, the formed groups
#' have a single male at least \code{minAge} old.
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @param sexRatio numeric value indicating the ratio of females to males x
#' (from 0.5 to 20 by increments of 0.5 within the accompanying Shiny
#' application. A sex ratio of 0 ignores sex in making up groups.
#' @noRd
fillGroupMembers <- function(candidates,
                             currentGroups,
                             kin,
                             ped,
                             harem,
                             minAge,
                             numGp,
                             sexRatio) {
  groupMembers <- makeGroupMembers(
    numGp, currentGroups, candidates, ped,
    harem, minAge
  )
  grpNum <- makeGrpNum(numGp)

  if (harem) {
    # Sires were added to groupMembers
    candidates <- removePotentialSires(candidates, minAge, ped)
  }
  if (sexRatio != 0.0) {
    groupMembers <- fillGroupMembersWithSexRatio(
      candidates,
      groupMembers,
      grpNum,
      kin,
      ped,
      minAge,
      numGp,
      sexRatio
    )
    return(groupMembers)
  } else {
    available <- makeAvailable(candidates, numGp)
  }

  repeat {
    if (isEmpty(grpNum)) {
      break
    }

    # Select a group at random
    i <- sample(grpNum, 1L)[[1L]]

    # Select an animal that can be added to this group and add it
    id <- sample(available[[i]], 1L)
    groupMembers[[i]] <- c(groupMembers[[i]], id)
    available <-
      removeSelectedAnimalFromAvailableAnimals(available, id, numGp)

    # Remove all relatives from consideration for the group it was added to
    available[[i]] <- setdiff(available[[i]], kin[[id]])
    grpNum <- removeGroupIfNoAvailableAnimals(grpNum, available)
  }
  groupMembers
}
