#' Filters kinship values from a long-format kinship table based on the sexes
#'  of the two animals involved.
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' Part of Group Formation
#'
#' @return A dataframe representing a filtered long-format kinship table.
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, and
#' \code{kinship}. This is the kinship data reformatted from a matrix,
#' to a long-format table.
#' @param ped Dataframe of pedigree information including the IDs listed in
#' \code{candidates}.
#' @param ignore a list containing zero or more character vectors of length 2
#' indicating which sex pairs should be ignored with regard to kinship.
#' Defaults to \code{list(c("F", "F"))}.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::lacy1989Ped
#' ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
#' kin <- kinMatrix2LongForm(kmat, removeDups = FALSE)
#' threshold <- 0.1
#' kin <- filterThreshold(kin, threshold = threshold)
#' ped$sex <- c("M", "F", "M", "M", "F", "F", "M")
#' kinNull <- filterPairs(kin, ped, ignore = NULL)
#' kinMM <- filterPairs(kin, ped, ignore = list(c("M", "M")))
#' ped
#' kin[kin$id1 == "C", ]
#' kinMM[kinMM$id1 == "C", ]
filterPairs <- function(kin, ped, ignore = list(c("F", "F"))) {
  if (length(ignore) == 0L) {
    return(kin)
  }
  kin["sort.col"] <- seq_len(nrow(kin))

  g1 <- merge(kin, ped, by.x = "id1", by.y = "id", all.x = TRUE, all.y = FALSE)
  g2 <- merge(kin, ped, by.x = "id2", by.y = "id", all.x = TRUE, all.y = FALSE)

  g1 <- g1[with(g1, order(sort.col)), "sex"]
  g2 <- g2[with(g2, order(sort.col)), "sex"]

  keep <- rep(TRUE, length(g1))

  for (i in seq_len(length(ignore))) {
    rel <- ignore[[i]]
    k <- !(((g1 == rel[1L]) &
      (g2 == rel[2L])) |
      ((g1 == rel[2L]) &
        (g2 == rel[1L])))
    keep <- keep & k
  }
  kin$sort.col <- NULL
  kin <- kin[keep, ]
  if (nrow(kin) > 0L) {
    rownames(kin) <- seq_len(nrow(kin))
  }
  return(kin[!is.na(kin[[1L]]), ])
}
