#' Calculates Founder Equivalents and Founder Genome Equivalents
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of the Genetic Value Analysis
#'
#' @return The list containing the founder equivalents,
#' \code{FE = 1 / sum(p ^ 2)}, and the founder genome equivalents,
#' \code{FG = 1 / sum( (p ^ 2) / r} where \code{p} is average number of
#' descendants and \code{r} is the mean number of founder alleles retained
#' in the gene dropping experiment.
#'
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#'
#' It is assumed that the pedigree has no partial parentage
#' @param alleles dataframe contains an \code{AlleleTable}. This is a
#' table of allele information produced by \code{geneDrop()}.
#' @export
#' @examples
#' data(lacy1989Ped)
#' ## Example from Analysis of Founder Representation in Pedigrees: Founder
#' ## Equivalents and Founder Genome Equivalents.
#' ## Zoo Biology 8:111-123, (1989) by Robert C. Lacy
#'
#' library(nprcgenekeepr)
#' ped <- nprcgenekeepr::lacy1989Ped
#' alleles <- lacy1989PedAlleles
#' pedFactors <- data.frame(
#'   id = as.factor(ped$id),
#'   sire = as.factor(ped$sire),
#'   dam = as.factor(ped$dam),
#'   gen = ped$gen,
#'   population = ped$population,
#'   stringsAsFactors = TRUE
#' )
#' allelesFactors <- geneDrop(pedFactors$id, pedFactors$sire, pedFactors$dam,
#'   pedFactors$gen,
#'   genotype = NULL, n = 5000,
#'   updateProgress = NULL
#' )
#' feFg <- calcFEFG(ped, alleles)
#' feFgFactors <- calcFEFG(pedFactors, allelesFactors)
calcFEFG <- function(ped, alleles) {
  ped <- toCharacter(ped, headers = c("id", "sire", "dam"))
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  # nolint start: commented_code_linter.
  ## UID.founders <- founders[grepl("^U", founders, ignore.case = TRUE)]
  # nolint end: commented_code_linter.
  ## UID.founders is not used; It may be a mistake, but it could be vestiges of
  ## something planned that was not done.
  descendants <- ped$id[!(ped$id %in% founders)]

  d <- matrix(0L, nrow = length(descendants), ncol = length(founders))
  colnames(d) <- founders
  rownames(d) <- descendants

  founderMatrix <- diag(length(founders))
  colnames(founderMatrix) <- rownames(founderMatrix) <- founders

  d <- rbind(founderMatrix, d)
  founderMatrix <- NULL
  ## Note: skips generation 0.
  ## The references inside matrix d do not work if ped$sire and ped$dam and
  ## thus gen$sire and gen$dam are factors. See test_calcFE.R
  for (i in seq_len(max(ped$gen))) {
    gen <- ped[(ped$gen == i), ]

    for (j in seq_len(nrow(gen))) {
      ego <- gen$id[j]
      sire <- gen$sire[j]
      dam <- gen$dam[j]
      d[ego, ] <- (d[sire, ] + d[dam, ]) / 2L
    }
  }

  currentDesc <- ped$id[ped$population & !(ped$id %in% founders)]
  d <- d[currentDesc, ]
  p <- colMeans(d)

  r <- calcRetention(ped, alleles)
  list(FE = 1L / sum(p^2L), FG = 1L / sum((p^2L) / r, na.rm = TRUE))
}
