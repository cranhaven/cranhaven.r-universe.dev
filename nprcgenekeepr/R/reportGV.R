#' Generates a genetic value report for a provided pedigree.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' This is the main function for the Genetic Value Analysis.
#'
#' @return A dataframe with the genetic value report. Animals are ranked
#' in order of descending value.
#'
#' @param ped The pedigree information in data.frame format
#' @param guIter Integer indicating the number of iterations for the gene-drop
#'  analysis. Default is 5000 iterations
#' @param guThresh Integer indicating the threshold number of animals for
#' defining a unique allele. Default considers an allele "unique"
#' if it is found in only 1 animal.
#' @param pop Character vector with animal IDs to consider as the population of
#' interest. The default is NULL.
#' @param byID Logical variable of length 1 that is passed through to
#' eventually be used by \code{alleleFreq()}, which calculates the count of each
#'  allele in the provided vector. If \code{byID} is TRUE and ids are provided,
#'  the function will only count the unique alleles for an individual
#'   (homozygous alleles will be counted as 1).
#' @param updateProgress Function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' examplePedigree <- nprcgenekeepr::examplePedigree
#' breederPed <- qcStudbook(examplePedigree,
#'   minParentAge = 2,
#'   reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#' focalAnimals <- breederPed$id[!(is.na(breederPed$sire) &
#'   is.na(breederPed$dam)) &
#'   is.na(breederPed$exit)]
#' ped <- setPopulation(ped = breederPed, ids = focalAnimals)
#' trimmedPed <- trimPedigree(focalAnimals, breederPed)
#' probands <- ped$id[ped$population]
#' ped <- trimPedigree(probands, ped,
#'   removeUninformative = FALSE,
#'   addBackParents = FALSE
#' )
#' geneticValue <- reportGV(ped,
#'   guIter = 50, # should be >= 1000
#'   guThresh = 3,
#'   byID = TRUE,
#'   updateProgress = NULL
#' )
#' trimmedGeneticValue <- reportGV(trimmedPed,
#'   guIter = 50, # should be >= 1000
#'   guThresh = 3,
#'   byID = TRUE,
#'   updateProgress = NULL
#' )
#' rpt <- trimmedGeneticValue[["report"]]
#' kmat <- trimmedGeneticValue[["kinship"]]
#' f <- trimmedGeneticValue[["total"]]
#' mf <- trimmedGeneticValue[["maleFounders"]]
#' ff <- trimmedGeneticValue[["femaleFounders"]]
#' nmf <- trimmedGeneticValue[["nMaleFounders"]]
#' nff <- trimmedGeneticValue[["nFemaleFounders"]]
#' fe <- trimmedGeneticValue[["fe"]]
#' fg <- trimmedGeneticValue[["fg"]]
reportGV <- function(ped, guIter = 5000L, guThresh = 1L, pop = NULL,
                     byID = TRUE, updateProgress = NULL) {
  # Generates a genetic value report for a provided pedigree

  ## If user has limited the population of interest by defining 'pop',
  ## that information is incorporated via the 'population' column.
  ped$population <- getGVPopulation(ped, pop)

  # Get the list of animals in the population to consider
  probands <- as.character(ped$id[ped$population])

  ## Extract genotype data if available otherwise NULL is returned.
  genotype <- getGVGenotype(ped)

  # Generate the kinship matrix and filter down to the animals of interest
  kmat <- filterKinMatrix(probands, kinship(
    ped$id, ped$sire, ped$dam,
    ped$gen
  ))

  # Calculate the mean kinship, and convert to z-scores
  indivMeanKin <- meanKinship(kmat)
  indivMeanKin <- indivMeanKin[probands] # making sure the order is correct
  zScores <- scale(indivMeanKin)

  # Perform the gene drop simulation
  alleles <- geneDrop(
    ids = ped$id, sires = ped$sire, dams = ped$dam,
    gen = ped$gen, genotype = genotype, n = guIter,
    updateProgress = updateProgress
  )

  if (!is.null(updateProgress)) {
    updateProgress(
      detail = "Calculating Genome Uniqueness", value = 1L,
      reset = TRUE
    )
  }

  # Calculate genome uniqueness and order the rows of the returned data.frame
  gu <- calcGU(alleles, threshold = guThresh, byID = byID, pop = probands)
  gu <- gu[probands, , drop = FALSE]

  if (!is.null(updateProgress)) {
    updateProgress(
      detail = "Calculating Numbers of Offspring", value = 1L,
      reset = TRUE
    )
  }

  # Get a data.frame of offspring counts for the probands
  offspring <- offspringCounts(probands, ped, considerPop = TRUE)

  includeCols <- intersect(getIncludeColumns(), names(ped))

  # Subsetting out the needed demographic information from the pedigree
  rownames(ped) <- ped$id
  demographics <- ped[probands, includeCols]

  if (!is.null(updateProgress)) {
    updateProgress(
      detail = "Calculating Founder Equivalents", value = 1L,
      reset = TRUE
    )
  }

  # Calculating founder equivalents and founder genome equivalents
  feFg <- calcFEFG(ped, alleles)

  # Calculating known founders
  founders <- ped[is.na(ped$sire) & is.na(ped$dam), ]
  males <- founders[(founders$sex == "M") & !grepl("^U", founders$id,
    ignore.case = TRUE
  ), ]
  females <- founders[(founders$sex == "F") & !grepl("^U", founders$id,
    ignore.case = TRUE
  ), ]

  finalData <- cbind(demographics, indivMeanKin, zScores, gu, offspring)
  finalData <- list(
    report = orderReport(finalData, ped),
    kinship = kmat,
    gu = gu,
    fe = feFg$FE,
    fg = feFg$FG,
    maleFounders = males,
    femaleFounders = females,
    nMaleFounders = nrow(males),
    nFemaleFounders = nrow(females),
    total = (nrow(males) + nrow(females))
  )
  class(finalData) <- append(class(finalData), "nprcgenekeeprGV")

  return(finalData)
}
