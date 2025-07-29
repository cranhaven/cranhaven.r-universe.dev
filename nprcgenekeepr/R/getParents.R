#' Get parents to corresponding animal IDs provided
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A character vector with the IDs of the parents of the provided ID
#' list.
#'
#' @param pedSourceDf dataframe with pedigree structure having at least the
#' columns id, sire, and dam.
#' @param ids character vector of animal IDs
#' @export
#' @examples
#' library(nprcgenekeepr)
#' pedOne <- nprcgenekeepr::pedOne
#' names(pedOne) <- c("id", "sire", "dam", "sex", "birth")
#' getParents(pedOne, c("o1", "d4"))
getParents <- function(pedSourceDf, ids) {
  unique(c(
    pedSourceDf$sire[(is.element(pedSourceDf$id, ids) &
      !is.na(pedSourceDf$sire))],
    pedSourceDf$dam[(is.element(pedSourceDf$id, ids) &
      !is.na(pedSourceDf$dam))]
  ))
}
