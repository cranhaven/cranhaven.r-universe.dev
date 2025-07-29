#' Recursively create a character vector of ancestors for an individual ID.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Sampling
#' From PedigreeSampling.R
#' 2016-01-28
#'
#' Contains functions to build pedigrees from sub-samples
#' of genotyped individuals.
#'
#' The goal of sampling is to reduce the number of inbreeding
#' loops in the resulting pedigree, and thus, reduce the
#' amount of time required to perform calculations with
#' SIMWALK2 or similar programs.
#'
#' @return A character vector of ancestors for an individual ID.
#'
#' @param id character vector of length 1 having the ID of interest
#' @param ptree a list of lists forming a pedigree tree as constructed by
#' \code{createPedTree(ped)} where \code{ped} is a standard pedigree dataframe.
#'
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::qcPed
#' ped <- qcStudbook(ped, minParentAge = 0)
#' pedTree <- createPedTree(ped)
#' pedLoops <- findLoops(pedTree)
#' ids <- names(pedTree)
#' allAncestors <- list()
#'
#' for (i in seq_along(ids)) {
#'   id <- ids[[i]]
#'   anc <- getAncestors(id, pedTree)
#'   allAncestors[[id]] <- anc
#' }
#' head(allAncestors)
#' countOfAncestors <- unlist(lapply(allAncestors, length))
#' idsWithMostAncestors <-
#'   names(allAncestors)[countOfAncestors == max(countOfAncestors)]
#' allAncestors[idsWithMostAncestors]
getAncestors <- function(id, ptree) {
  if (is.na(id)) {
    return(character(0L))
  }

  sire <- ptree[[id]]$sire
  dam <- ptree[[id]]$dam

  if (!is.na(sire)) {
    sAnc <- getAncestors(sire, ptree)
    sireLineage <- c(sire, sAnc)
  } else {
    sireLineage <- character(0L)
  }

  if (!is.na(dam)) {
    dAnc <- getAncestors(dam, ptree)
    damLineage <- c(dam, dAnc)
  } else {
    damLineage <- character(0L)
  }

  c(sireLineage, damLineage)
}
