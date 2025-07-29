#' Remove group numbers when all available animals have been used
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return The \code{grpNum} list after removing any list element corresponding
#' to a group with no available animals left using in filling a group.
#'
#' @param grpNum as list of integer vectors initially populated with one list
#' named by the integers 1:\code{numGrp}, where \code{numGrp} is the number of
#' groups to be formed. Each list member is initially populated with a integer
#' vector \code{seq_len(numGrp)}.
#' @param available is a list of numGrp named members and each member is
#' initially defined as the character vector made up of candidate animal Ids.
#' @noRd
removeGroupIfNoAvailableAnimals <- function(grpNum, available) { # nolint: object_length_linter
  remainingGrpNum <- grpNum
  for (i in remainingGrpNum) {
    if (isEmpty(available[[i]])) {
      grpNum <- setdiff(grpNum, i)
    }
  }
  grpNum
}
