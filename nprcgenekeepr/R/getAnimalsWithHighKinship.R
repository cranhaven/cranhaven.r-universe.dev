#' Forms a list of animal Ids and animals related to them
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A list of named character vectors where each name is an animal Id
#' and the character vectors are made up of animals sharing a kinship value
#' greater than our equal to the \code{threshold} value.
#'
#' @param kmat numeric matrix of pairwise kinship values. Rows and columns
#' are named with animal IDs.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param threshold numeric value indicating the minimum kinship level to be
#' considered in group formation. Pairwise kinship below this level will be
#' ignored.
#' @param currentGroups list of character vectors of IDs of animals currently
#' assigned
#' to the group. Defaults to character(0) assuming no groups are existent.
#' @param ignore list of character vectors representing the sex combinations
#' to be ignored. If provided, the vectors in the list specify if pairwise
#' kinship should be ignored between certain sexes.
#' Default is to ignore all pairwise kinship between females.
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#'
#' @export
#' @examples
#' qcPed <- nprcgenekeepr::qcPed
#' ped <- qcStudbook(qcPed,
#'   minParentAge = 2L, reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
#' currentGroups <- list(1L)
#' currentGroups[[1L]] <- examplePedigree$id[1L:3L]
#' candidates <- examplePedigree$id[examplePedigree$status == "ALIVE"]
#' threshold <- 0.015625
#' kin <- getAnimalsWithHighKinship(kmat, ped, threshold, currentGroups,
#'   ignore = list(c("F", "F")), minAge = 1.0
#' )
#' length(kin) # should be 259
#' kin[["0DAV0I"]] # should have 34 IDs
getAnimalsWithHighKinship <- function(kmat, ped, threshold, currentGroups,
                                      ignore, minAge) {
  kin <- kinMatrix2LongForm(kmat)

  kin <- filterThreshold(kin, threshold = threshold)
  kin <- filterPairs(kin, ped, ignore = ignore)
  kin <- filterAge(kin, ped, minAge = minAge)

  # Filter out self kinships
  kin <- kin[(kin$id1 != kin$id2), ]

  # Ignore kinship between current group members
  kin <- kin[!((kin$id1 %in% unlist(currentGroups)) &
    (kin$id2 %in% unlist(currentGroups))), ]

  # Converting the kinships to a list
  kin <- tapply(kin$id2, kin$id1, c)
  kin
}
