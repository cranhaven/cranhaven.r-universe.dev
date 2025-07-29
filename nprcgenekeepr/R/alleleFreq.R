#' Calculates the count of each allele in the provided vector.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Genetic Value Analysis
#'
#'  If ids are provided, the function will only count the unique alleles
#' for an individual (homozygous alleles will be counted as 1).
#'
#'
#' @return A data.frame with columns \code{allele} and \code{freq}. This is a
#'  table of allele counts within the population.
#'
#' @param alleles an integer vector of alleles in the population
#' @param ids character vector of IDs indicating to which animal each allele
#' in \code{alleles} belongs.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' data("ped1Alleles")
#' ids <- ped1Alleles$id
#' alleles <- ped1Alleles[, !(names(ped1Alleles) %in% c("id", "parent"))]
#' aF <- alleleFreq(alleles[[1]], ids = NULL)
#' aF[aF$freq >= 10, ]
alleleFreq <- function(alleles, ids = NULL) {
  if (!is.null(ids)) {
    alleles <- unlist(tapply(alleles, as.factor(ids), unique))
  }

  a <- as.data.frame(table(alleles))
  colnames(a) <- c("allele", "freq")
  return(a)
}
