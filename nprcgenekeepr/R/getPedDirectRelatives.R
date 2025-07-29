#' Get the direct ancestors of selected animals from supplied pedigree.
#'
## Copyright(c) 2017-2023 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Gets direct ancestors from labkey \code{study} schema and \code{demographics}
#' table.
#'
#' @return A data.frame with pedigree structure having all of the direct
#' ancestors for the Ids provided.
#'
#' @param ids character vector with Ids.
#' @param ped pedigree dataframe object that is used as the source of
#' pedigree information.
#' @param unrelatedParents logical vector when \code{FALSE} the unrelated
#' parents of offspring do not get a record as an ego; when \code{TRUE}
#' a place holder record where parent (\code{sire},
#' \code{dam}) IDs are set to \code{NA}.
#'
#' @import futile.logger
#' @importFrom data.table rbindlist
#' @importFrom stringi stri_c
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ## Have to a vector of focal animals
#' focalAnimals <- c("1X2701", "1X0101")
#' suppressWarnings(getLkDirectRelatives(ids = focalAnimals))
getPedDirectRelatives <- function(ids, ped, unrelatedParents = FALSE) {
  if (missing(ids)) {
    stop("Need to specify IDs in 'id' parameter.")
  }

  if (missing(ped)) {
    stop("Need to specify pedigree in 'ped' parameter.")
  }

  if (is.null(ped)) {
    return(NULL)
  }

  if (!is.data.frame(ped)) {
    stop("ped must be a data.frame object.")
  }


  offspring <- parents <- ids
  len <- length(ids)
  while (len > 0L) {
    parents <- getParents(ped, ids)
    offspring <- getOffspring(ped, ids)
    added <- unique(union(parents, offspring))
    added <- setdiff(added, ids)
    len <- length(added)
    if (len == 0L) {
      break
    }
    ids <- union(added, ids)
    ids <- ids[!is.na(ids)]
  }

  if (unrelatedParents) {
    unrelated <- unique(ids[!ids %in% ped$id])
    unrelated <- unrelated[!is.na(unrelated)]
    addIdRecords(
      ids = unrelated, fullPed = ped,
      partialPed = ped[ped$id %in% ids, ]
    )
  }
  ped[ped$id %in% ids, ]
}
