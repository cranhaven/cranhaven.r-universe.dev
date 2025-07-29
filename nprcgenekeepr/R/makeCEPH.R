#' Make a CEPH-style pedigree for each id
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Relations
#'
#' Creates a CEPH-style pedigree for each id, consisting of three generations:
#' the id, the parents, and the grandparents. Inserts NA for unknown pedigree
#' members.
#'
#' Calculates the first-order relationships in a pedigree,
#' and to convert pairwise kinships to the appropriate relationship category.

#' Relationships categories:
#' For each ID in the pair, find a CEPH-style pedigree and compare them
#' \itemize{
#' \item \{If one is the parent of the other\}
#'     \{--- Designate the relationship as \code{parent-offspring}\}
#' \item \{Else if both parents are shared\}
#'     \{--- Designate the relationship as \code{full-siblings}\}
#' \item \{Else if one parent is shared\}
#'     \{--- Designate the relationship as \code{half-siblings}\}
#' \item \{Else if one is the grandparent of the other\}
#'     \{--- Designate the relationship as \code{grandparent-grandchild}\}
#' \item \{Else if both grand parents are shared\}
#'     \{--- Designate the relationship as \code{cousin}\}
#' \item \{Else if at least one grand parent is shared\}
#'     \{--- Designate the relationship as \code{cousin - other}\}
#' \item \{Else if the parents of one are the grandparents of the other\}
#'     \{--- Designate the relationship as \code{full-avuncular}\}
#' \item \{Else if a single parent of one is the grandparent of the other\}
#'     \{--- Designate the relationship as \code{avuncular - other}\}
#' \item \{Else if the kinship is greater than 0, but the pair don't fall into
#' the above categories\}
#'     \{--- Designate the relationship as \code{other}\}
#' \item \{Else\}
#'     \{--- Designate the relationships as \code{no relation.}\}
#' }
#'
#' @return List of lists: \{fields: id, \{subfields: parents, pgp, mgp\}\}.
#' Pedigree information converted into a CEPH-style list. The top level
#' list elements are the IDs from id. Below each ID is a list of three
#' elements: parents (sire, dam), paternal grandparents (pgp: sire, dam),
#' and maternal grandparents (mgp: sire, dam).
#'
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::lacy1989Ped
#' pedCEPH <- makeCEPH(ped$id, ped$sire, ped$dam)
#' head(ped)
#' head(pedCEPH$F)
makeCEPH <- function(id, sire, dam) {
  ped <- data.frame(
    sire = sire, dam = dam, row.names = id,
    stringsAsFactors = FALSE
  )

  ceph <- list()
  for (i in id) {
    sire <- ped[i, "sire"]
    dam <- ped[i, "dam"]
    parents <- c(sire, dam)

    if (is.na(sire)) {
      pgp <- c(NA, NA)
    } else {
      pgp <- c(ped[sire, "sire"], ped[sire, "dam"])
    }

    if (is.na(dam)) {
      mgp <- c(NA, NA)
    } else {
      mgp <- c(ped[dam, "sire"], ped[dam, "dam"])
    }

    ceph[[i]] <- list(parents = parents, pgp = pgp, mgp = mgp)
  }

  ceph
}
