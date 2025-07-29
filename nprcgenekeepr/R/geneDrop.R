#' Gene drop simulation based on the provided pedigree information
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Genetic Value Analysis
#'
#' The gene dropping method from \emph{Pedigree analysis by computer simulation}
#' by Jean W MacCluer, John L Vandeberg, and Oliver A Ryder (1986)
#' <doi:10.1002/zoo.1430050209> is used in the genetic value calculations.
#'
#' Currently there is no means of handling knowing only one haplotype.
#' It will be easy to add another column to handle situations where only one
#' allele is observed and it is not known to be homozygous or heterozygous. The
#' new fourth column could have a frequency for homozygosity that could be
#' used in the gene dropping algorithm.
#'
#' The genotypes are using indirection (integer instead of character) to
#' indicate the genes because the manipulation of character strings was found
#' to take 20-35 times longer to perform.
#'
#' Adding additional columns to \code{genotype} does not significantly affect
#' the time require. Thus, it is convenient to add the corresponding haplotype
#' names to the dataframe using \code{first_name} and \code{second_name}.

#' @return A data.frame \code{id, parent, V1 ... Vn}
#' A data.frame providing the maternal and paternal alleles for an animal
#' for each iteration. The first two columns provide the animal's ID and
#' whether the allele came from the sire or dam. These are followed by
#' \code{n} columns indicating the allele for that iteration.
#'
#' @param ids A character vector of IDs for a set of animals.
#' @param sires A character vector with IDS of the sires for the set of
#'  animals. \code{NA} is used for missing sires.
#' @param dams A character vector with IDS of the dams for the set of
#'  animals. \code{NA} is used for missing dams.
#' @param gen An integer vector indicating the generation number for each
#' animal.
#' @param genotype A dataframe containing known genotypes. It has three
#' columns:  \code{id}, \code{first}, and \code{second}. The second and third
#' columns contain the integers indicating the observed genotypes.
#'
#' @param n integer indicating the number of iterations to simulate.
#' Default is 5000.
#' @param updateProgress function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#'
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
geneDrop <- function(ids, sires, dams, gen, genotype = NULL, n = 5000L,
                     updateProgress = NULL) {
  ## Sort the IDs by generation so older generations are first
  ped <- data.frame(
    id = ids, sire = sires, dam = dams, gen,
    stringsAsFactors = FALSE
  )
  ped <- toCharacter(ped, headers = c("id", "sire", "dam"))
  rownames(ped) <- ids
  ped <- ped[order(gen), ]
  if (!is.null(genotype)) {
    genotype <- genotype[!(is.na(genotype$first) & is.na(genotype$second)), ]
    genoDefined <- TRUE
  } else {
    genoDefined <- FALSE
  }

  alleles <- list(alleles = list(), counter = 1L)

  if (!is.null(updateProgress)) {
    updateProgress(
      detail = "Performing Gene-drop Simulation", value = 0L,
      reset = TRUE
    )
  }

  ## Iterate through each ID and get the maternal and paternal alleles
  for (id in ped$id) {
    alleles$alleles[[id]] <- list()
    sire <- ped[id, "sire"]
    dam <- ped[id, "dam"]
    assigned <- FALSE
    if (genoDefined && any(genotype$id == id)) {
      alleles <- getGenoDefinedParentGenotypes(alleles, genotype, id, sire,
                                               dam, n)
      assigned <- TRUE
    }
    if (!assigned) {
      alleles <- assignAlleles(alleles, "sire", sire, id, n)
      alleles <- assignAlleles(alleles, "dam", dam, id, n)
    }

    if (!is.null(updateProgress)) {
      updateProgress(n = nrow(ped))
    }
  }

  # Convert the list of alleles to a data.frame
  alleles <- as.data.frame(t(data.frame(alleles$alleles, check.names = FALSE)))
  keys <- strsplit(rownames(alleles), ".", fixed = TRUE)

  id <- character(0L)
  parent <- character(0L)
  for (i in seq_len(length(keys))) {
    key <- keys[[i]]
    id <- c(id, key[1L])
    parent <- c(parent, key[2L])
  }

  alleles$id <- id
  alleles$parent <- parent
  rownames(alleles) <- seq_len(nrow(alleles))
  return(alleles)
}
