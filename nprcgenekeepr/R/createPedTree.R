#' Create a pedigree tree (PedTree).
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' The PedTree is a list containing sire and dam information for an individual.
#'
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
#'
#' This function uses only \code{id}, \code{sire}, and \code{dam} columns.
#'
#' @return A list of named lists forming a pedigree tree (PedTree or ptree).
#' Each sublist represents an ID in the pedigree and contains the sire ID and
#' the dam ID as named elements.
#'
#' @param ped dataframe of pedigree and demographic information potentially
#' containing columns indicating the birth and death dates of an individual.
#' The table may also contain dates of sale (departure). Optional columns
#' are \code{birth}, \code{death}, \code{departure}.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' exampleTree <- createPedTree(nprcgenekeepr::examplePedigree)
#' exampleLoops <- findLoops(exampleTree)
createPedTree <- function(ped) {
  pedTree <- rep(list(list(sire = NA, dam = NA)), nrow(ped))
  names(pedTree) <- ped$id

  for (i in seq_len(nrow(ped))) {
    pedTree[[ped$id[i]]]$sire <- ped$sire[i]
    pedTree[[ped$id[i]]]$dam <- ped$dam[i]
  }
  pedTree
}
