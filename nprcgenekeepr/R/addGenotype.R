#' Add genotype data to pedigree file
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Assumes genotype has been opened by \code{checkGenotypeFile}
#'
#' @return A pedigree object with genotype data added.
#'
#' @examples
#' library(nprcgenekeepr)
#' rhesusPedigree <- nprcgenekeepr::rhesusPedigree
#' rhesusGenotypes <- nprcgenekeepr::rhesusGenotypes
#' pedWithGenotypes <- addGenotype(
#'   ped = rhesusPedigree,
#'   genotype = rhesusGenotypes
#' )
#'
#' @param ped pedigree dataframe. \code{ped} is to be provided by
#' \code{qcStudbook} so it is not checked.
#' @param genotype genotype dataframe. \code{genotype} is to be provided by
#' \code{checkGenotypeFile} so it is not checked.
#' @export
addGenotype <- function(ped, genotype) {
  genotypeNames <- names(genotype)[2L:3L]
  geno <- sort(unique(c(
    genotype[, genotypeNames[1L]],
    genotype[, genotypeNames[2L]]
  )))
  genoDict <- seq_along(geno) + 10000L
  names(genoDict) <- geno
  genotype <- cbind(genotype,
    first = as.integer(genoDict[genotype[, 2L]]),
    second = as.integer(genoDict[genotype[, 3L]])
  )
  newPed <- merge(ped, genotype, by = "id", all = TRUE)
  newPed
}
