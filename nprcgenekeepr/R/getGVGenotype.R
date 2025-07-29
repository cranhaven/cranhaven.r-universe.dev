#' Get Genetic Value Genotype data structure for reportGV function.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Extracts genotype data if available otherwise NULL is returned.
#'
#' @return A data.frame with the columns \code{id}, \code{first}, and
#' \code{second} extracted from a pedigree object (a data.frame) containing
#' genotypic data.
#' If the pedigree object does not contain genotypic data the \code{NULL} is
#' returned.
#'
#' @param ped the pedigree information in datatable format
#' @export
#' @examples
#' ## We usually defined `n` to be >= 5000
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::lacy1989Ped
#' allelesNew <- geneDrop(ped$id, ped$sire, ped$dam, ped$gen,
#'   genotype = NULL, n = 50, updateProgress = NULL
#' )
#' genotype <- data.frame(
#'   id = ped$id,
#'   first_allele = c(
#'     NA, NA, "A001_B001", "A001_B002",
#'     NA, "A001_B002", "A001_B001"
#'   ),
#'   second_allele = c(
#'     NA, NA, "A010_B001", "A001_B001",
#'     NA, NA, NA
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' pedWithGenotype <- addGenotype(ped, genotype)
#' pedGenotype <- getGVGenotype(pedWithGenotype)
#' allelesNewGen <- geneDrop(ped$id, ped$sire, ped$dam, ped$gen,
#'   genotype = pedGenotype,
#'   n = 5, updateProgress = NULL
#' )
getGVGenotype <- function(ped) {
  if (hasGenotype(ped)) {
    genotype <- ped[, c("id", "first", "second")]
  } else {
    genotype <- NULL
  }
  genotype
}
