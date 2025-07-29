#' Filters kinship to remove rows with kinship values less than the specified
#' threshold
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' Part of Group Formation
#' Filters kinship values less than the specified threshold from a long-format
#' table of kinship values.
#'
#' @return The kinship matrix with all kinship relationships below the
#' threshold value removed.
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, and
#' \code{kinship}. This is the kinship data reformatted from a matrix,
#' to a long-format table.
#' @param threshold numeric value representing the minimum kinship level to be
#' considered in group formation. Pairwise kinship below this level will be
#' ignored.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::lacy1989Ped
#' ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
#' kin <- kinMatrix2LongForm(kmat, removeDups = FALSE)
#' kinFiltered_0.3 <- filterThreshold(kin, threshold = 0.3)
#' kinFiltered_0.1 <- filterThreshold(kin, threshold = 0.1)
filterThreshold <- function(kin, threshold = 0.015625) {
  kin <- kin[kin$kinship >= threshold, ]
  if (nrow(kin) > 0L) {
    rownames(kin) <- seq_len(nrow(kin))
  }
  return(kin)
}
