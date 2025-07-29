#' Count first-order relatives.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Relations
#'
#' Tallies the number of first-order relatives for each member of the provided
#' pedigree. If 'ids' is provided, the analysis is restricted to only the
#' specified subset.
#'
#' @return A dataframe with column \code{id}, \code{parents}, \code{offspring},
#' \code{siblings}, and \code{total}. A table of first-order relationship
#' counts, broken down to indicate the number of parents, offspring, and
#' siblings that are part of the subset under consideration.
#'
#' @param ped : `Pedigree`
#'   Standardized pedigree information in a table.
#' @param ids character vector of IDs or NULL
#'   These are the IDs to which the analysis should be restricted. First-order
#'   relationships will only be tallied for the listed IDs and will only
#'   consider relationships within the subset. If NULL, the analysis will
#'   include all IDs in the pedigree.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::lacy1989Ped
#' ids <- c("B", "D", "E", "F", "G")
#' countIds <- countFirstOrder(ped, ids)
#' countIds
#' count <- countFirstOrder(ped, NULL)
#' count
countFirstOrder <- function(ped, ids = NULL) {
  if (!is.null(ids)) {
    ped <- ped[ped$id %in% ids, ]
  }
  rownames(ped) <- seq_len(nrow(ped))
  parents <- integer(0L)
  offspring <- integer(0L)
  siblings <- integer(0L)

  for (i in seq_len(nrow(ped))) {
    id <- ped[i, "id"]
    sire <- ped[i, "sire"]
    dam <- ped[i, "dam"]

    p <- sum(c((sire %in% ped$id), (dam %in% ped$id)))
    o <- sum((ped$sire %in% id) | (ped$dam %in% id))
    if (is.na(sire) || is.na(dam)) {
      s <- 0L
    } else {
      s <- sum((ped$sire %in% sire) & (ped$dam %in% dam)) - 1L
    }

    parents <- c(parents, p)
    offspring <- c(offspring, o)
    siblings <- c(siblings, s)
  }
  total <- parents + offspring + siblings
  ped["parents"] <- parents
  ped["offspring"] <- offspring
  ped["siblings"] <- siblings
  ped["total"] <- total

  return(ped[, c("id", "parents", "offspring", "siblings", "total")])
}
