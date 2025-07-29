#' Converts pairwise kinship values to a relationship category descriptor.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Relations
#'
#' @return A dataframe with columns \code{id1}, \code{id2}, \code{kinship},
#' \code{relation}. It is a long-form table of pairwise kinships, with
#' relationship categories included for each pair.
#'
#' @param kmat a numeric matrix of pairwise kinship coefficients.
#' Rows and columns should be named with IDs.
#' @param ped the pedigree information in datatable format with required
#' colnames \code{id}, \code{sire}, and \code{dam}.
#' @param ids character vector of IDs or NULL to which the analysis should be
#' restricted. If provided, only relationships between these IDs will be
#' converted to relationships.
#' @param updateProgress function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::smallPed
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
#' ids <- c("A", "B", "D", "E", "F", "G", "I", "J", "L", "M", "O", "P")
#' relIds <- convertRelationships(kmat, ped, ids)
#' rel <- convertRelationships(kmat, ped, updateProgress = function() {})
#' head(rel)
#' ped <- nprcgenekeepr::qcPed
#' bkmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen,
#'   sparse = FALSE
#' )
#' relBIds <- convertRelationships(bkmat, ped, c("4LFS70", "DD1U77"))
#' relBIds
convertRelationships <- function(kmat, ped, ids = NULL, updateProgress = NULL) {
  if (!is.null(ids)) {
    kmat <- filterKinMatrix(ids, kmat)
  }
  kin <- kinMatrix2LongForm(kmat, removeDups = TRUE)
  ped <- makeCEPH(ped$id, ped$sire, ped$dam)
  r <- character(0L)

  for (i in seq_len(nrow(kin))) {
    id1 <- kin$id1[i]
    id2 <- kin$id2[i]

    ceph1 <- ped[[id1]]
    ceph2 <- ped[[id2]]

    if (id1 == id2) {
      relation <- "Self"
    } else if (allTrueNoNA(ceph1$parents == ceph2$parents)) {
      relation <- "Full-Siblings"
    } else if (id1 %in% ceph2$parents || id2 %in% ceph1$parents) {
      # one animal is the parent of the other
      relation <- "Parent-Offspring"
    } else if (!isEmpty(intersect(ceph1$parents, ceph2$parents))) {
      # at least 1 parent is shared
      relation <- "Half-Siblings"
    } else if (id1 %in% c(ceph2$pgp, ceph2$mgp) ||
      id2 %in% c(ceph1$pgp, ceph1$mgp)) {
      # one animals is the grandparent of the other
      relation <- "Grandparent-Grandchild"
    } else if (allTrueNoNA(ceph1$pgp == ceph2$pgp) ||
      allTrueNoNA(ceph1$pgp == ceph2$mgp) ||
      allTrueNoNA(ceph1$mgp == ceph2$pgp) ||
      allTrueNoNA(ceph1$mgp == ceph2$mgp)) {
      # When a full set of grandparents are shared
      relation <- "Full-Cousins"
    } else if (!isEmpty(intersect(
      c(ceph1$pgp, ceph1$mgp),
      c(ceph2$pgp, ceph2$mgp)
    ))) {
      # When at least one grandparent is in common
      relation <- "Cousin - Other"
    } else if (allTrueNoNA(ceph1$parents == ceph2$pgp) ||
      allTrueNoNA(ceph1$parents == ceph2$mgp) ||
      allTrueNoNA(ceph2$parents == ceph1$pgp) ||
      allTrueNoNA(ceph2$parents == ceph1$mgp)) {
      # When parents of one proband are the grandparents of the other
      relation <- "Full-Avuncular"
    } else if (!isEmpty(intersect(ceph1$parents, c(ceph2$pgp, ceph2$mgp))) ||
      !isEmpty(intersect(ceph2$parents, c(ceph1$pgp, ceph1$mgp)))) {
      # When at least one parent of a proband is the grandparent of the other
      relation <- "Avuncular - Other"
    } else if (kin$kinship[i] > 0L) {
      relation <- "Other"
    } else {
      relation <- "No Relation"
    }

    r <- c(r, relation)

    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }
  kin["relation"] <- r
  return(kin)
}
