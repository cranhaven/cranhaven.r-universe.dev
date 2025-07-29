#' Adds an NA value for all animals without a relative
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' This allows \code{kin} to be used with \code{setdiff} when there are no
#' relatives otherwise an error would occur because
#' \code{kin[['animal_with_no_relative']]} would not be found. See the
#' following: in \strong{groupAddAssign}
#'
#'     \code{available[[i]] <- setdiff(available[[i]], kin[[id]])}
#'
#' @return A dataframe with kinships in long form after adding a row for each
#' animal without a relative.
#'
#' @param kin dataframe with kinship values
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' qcPed <- nprcgenekeepr::qcPed
#' ped <- qcStudbook(qcPed,
#'   minParentAge = 2.0, reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
#' currentGroups <- list(1L)
#' currentGroups[[1]] <- examplePedigree$id[1:3]
#' candidates <- examplePedigree$id[examplePedigree$status == "ALIVE"]
#' threshold <- 0.015625
#' kin <- getAnimalsWithHighKinship(kmat, ped, threshold, currentGroups,
#'   ignore = list(c("F", "F")), minAge = 1.0
#' )
#' # Filtering out candidates related to current group members
#' conflicts <- unique(c(
#'   unlist(kin[unlist(currentGroups)]),
#'   unlist(currentGroups)
#' ))
#' candidates <- setdiff(candidates, conflicts)
#' kin <- addAnimalsWithNoRelative(kin, candidates)
#' length(kin) # should be 259
#' kin[["0DAV0I"]] # should have 34 IDs
addAnimalsWithNoRelative <- function(kin, candidates) {
  # adding animals with no relatives
  for (cand in setdiff(candidates, names(kin))) {
    kin[[cand]] <- NA
  }
  kin
}
