#' Find loops in a pedigree tree
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
#' @return A named list of logical values where each named element is
#' named with an \code{id} from \code{ptree}. The value of the list element
#' is set to \code{TRUE} if the \code{id} has a loop in the pedigree.
#' Loops occur when an animal's sire and dam have a common ancestor.
#'
#' @param ptree a list of lists forming a pedigree tree as constructed by
#' \code{createPedTree(ped)} where \code{ped} is a standard pedigree dataframe.
#' @export
#' @examples
#' data("examplePedigree")
#' exampleTree <- createPedTree(examplePedigree)
#' exampleLoops <- findLoops(exampleTree)
findLoops <- function(ptree) {
  ids <- names(ptree)
  loops <- vector("list", length(ids))
  names(loops) <- ids

  for (id in ids) {
    if (makesLoop(id, ptree)) {
      loops[[id]] <- TRUE
    } else {
      loops[[id]] <- FALSE
    }
  }
  loops
}
