#' Assign parent alleles randomly
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return The original list \code{alleles} passed into the function with newly
#' randomly assigned alleles to each \code{id} based on dam and sire genotypes.
#'
#' @param alleles a list with a list \code{alleles$alleles}, which is a list
#' of list containing the alleles for each individual's sire and dam that have
#' been assigned thus far and \code{alleles$counter} that is the counter used
#' to track the lists of\code{alleles$alleles}.
#' @param parentType character vector of length one with value of
#' \code{"sire"} or \code{"dam"}.
#' @param parent either \code{ped[id, "sire"]} or \code{ped[id, "dam"]}.
#' @param id character vector of length one containing the animal ID
#' @param n integer indicating the number of iterations to simulate.
#' Default is 5000.
#' @export
#' @examples
#' alleles <- list(alleles = list(), counter = 1)
#' alleles <- assignAlleles(alleles,
#'   parentType = "sire", parent = NA,
#'   id = "o1", n = 4
#' )
#' alleles
#' alleles <- assignAlleles(alleles,
#'   parentType = "dam", parent = NA,
#'   id = "o1", n = 4
#' )
#' alleles
assignAlleles <- function(alleles, parentType, parent, id, n) {
  if (is.na(parent)) {
    # If the parent is unknown, create a unique set of alleles for him or her
    alleles$alleles[[id]][[parentType]] <- rep(alleles$counter, n)
    alleles$counter <- alleles$counter + 1L
  } else {
    if (is.null(alleles$alleles[[parent]][["sire"]]) ||
      is.null(alleles$alleles[[parent]][["dam"]])) {
      stop("sire and dam must have had alleles assigned: logic error")
    }
    # Otherwise get his two sets of alleles and randomly select one
    # for each iteration
    p1 <- alleles$alleles[[parent]][["sire"]]
    p2 <- alleles$alleles[[parent]][["dam"]]
    alleles$alleles[[id]][[parentType]] <- chooseAlleles(p1, p2)
  }
  alleles
}
