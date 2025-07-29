#' Filters a kinship matrix to include only the egos listed in 'ids'
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A numeric matrix that is the reduced kinship matrix with named
#' rows and columns (row and col names are 'ids').
#'
#' @param ids character vector containing the IDs of interest.
#' The kinship matrix should be reduced to only include these rows and columns.
#' @param kmat a numeric matrix of pairwise kinship coefficients.
#' Rows and columns should be named with IDs.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::qcPed
#' ped$gen <- findGeneration(ped$id, ped$sire, ped$dam)
#' kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen,
#'   sparse = FALSE
#' )
#' ids <- ped$id[c(189, 192, 194, 195)]
#' ncol(kmat)
#' nrow(kmat)
#' kmatFiltered <- filterKinMatrix(ids, kmat)
#' ncol(kmatFiltered)
#' nrow(kmatFiltered)
filterKinMatrix <- function(ids, kmat) {
  kmat[(rownames(kmat) %in% ids), (colnames(kmat) %in% ids)]
}
