#' Convenience function to make the initial groupMembers animal list
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Initial groupMembers list
#'
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @param currentGroups list of character vectors of IDs of animals currently
#' assigned to the group. Defaults to character(0) assuming no groups are
#' existent.
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param harem logical variable when set to \code{TRUE}, the formed groups
#' have a single male at least \code{minAge} old.
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @export
makeGroupMembers <- function(numGp, currentGroups, candidates, ped, harem,
                             minAge) {
  groupMembers <- list()
  if (harem) {
    ## Since harems only have a single male, they are inserted during
    ## initialization.
    groupMembers <- initializeHaremGroups(
      numGp, currentGroups, candidates,
      ped, minAge
    )
  } else {
    for (i in seq_len(numGp)) {
      if (length(currentGroups) >= i) {
        groupMembers[[i]] <- currentGroups[[i]]
      } else {
        groupMembers[[i]] <- vector()
      }
    }
  }
  groupMembers
}
