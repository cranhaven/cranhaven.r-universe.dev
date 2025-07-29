#' Assigns parental genotype contributions to an IDs genotype by attributing
#' alleles to sire or dam
#'
#' @param alleles data.frame \code{id, parent, V1 ... Vn}
#' A data.frame providing the maternal and paternal alleles for an animal
#' for each iteration. The first two columns provide the animal's ID and
#' whether the allele came from the sire or dam. These are followed by
#' \code{n} columns indicating the allele for that iteration.
#' @param genotype A dataframe containing known genotypes. It has three
#' columns:  \code{id}, \code{first}, and \code{second}. The second and third
#' columns contain the integers indicating the observed genotypes.
#' @param id A character vector of length one having the ID of interest
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @param n integer indicating the number of iterations to simulate.
#' @return data.frame \code{id, parent, V1 ... Vn}
#' A data.frame providing the maternal and paternal alleles for an animal
#' for each iteration. The first two columns provide the animal's ID and
#' whether the allele came from the sire or dam. These are followed by
#' \code{n} columns indicating the allele for that iteration.
#'
#' This is not correct for situations where one haplotype is not known.
#' @noRd
getGenoDefinedParentGenotypes <- function(alleles, genotype, id, sire, dam, n) {
  if (is.na(genotype$first[genotype$id == id])) {
    alleles <- assignAlleles(alleles, "sire", sire, id, n)
  } else {
    alleles$alleles[[id]][["sire"]] <-
      rep(genotype$first[genotype$id == id], n)
  }
  if (is.na(genotype$second[genotype$id == id])) {
    alleles <- assignAlleles(alleles, "dam", dam, id, n)
  } else {
    alleles$alleles[[id]][["dam"]] <-
      rep(genotype$second[genotype$id == id], n)
  }
  alleles
}
