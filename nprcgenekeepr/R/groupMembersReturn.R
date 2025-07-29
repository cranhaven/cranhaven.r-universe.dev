#' Forms return list of groupAddAssign function
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A list with two or three elements \code{group}, \code{score}, and
#' optionally \code{groupKin} where
#' \code{group} is set to \code{savedGroupMembers}, \code{score} is set to
#' \code{savedScore}, and if \code{withKin == TRUE} \code{groupKin} is set to
#' a list of kinship matrices for each individual in \code{savedGroupMembers}.
#'
#' @param savedGroupMembers selected animal group
#' @param savedScore score of selected group, which is the group having the
#'  largest minimum group size
#' @param withKin logical variable indicating to return kinship coefficients
#' when \code{TRUE}.
#' @param kmat numeric matrix of pairwise kinship values. Rows and columns
#' are named with animal IDs.
#' @noRd
groupMembersReturn <- function(savedGroupMembers, savedScore, withKin, kmat) {
  if (withKin) {
    groupKin <- list()
    for (i in seq_along(savedGroupMembers)) {
      groupKin[[i]] <- filterKinMatrix(savedGroupMembers[[i]], kmat)
    }
    value <- list(
      group = savedGroupMembers, score = savedScore,
      groupKin = groupKin
    )
  } else {
    value <- list(group = savedGroupMembers, score = savedScore)
  }
  value
}
