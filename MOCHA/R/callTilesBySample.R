#' @title \code{callTilesBySample}
#'
#' @description \code{callTilesBySample} is the main peak-calling function in MOCHA
#'   that serves as a wrapper function to call peaks provided a set of fragment
#'   files and cell metadata
#'
#' @param blackList A GRanges object containing a blacklist of regions to
#'   exclude
#' @param returnAllPeaks boolean. Indicates whether MOCHA should return object
#'   containing all genomic regions or just the positive (+) called peaks.
#'   Default to the latter, only positive peaks.
#' @param numCores integer. Number of cores to parallelize peak-calling across
#'   multiple cell populations
#'
#' @param totalFrags # of fragments in that sample for that cell population
#' @param StudypreFactor ratio of average signal between new study and training
#'   set
#' @return scMACs_PeakList an list containing peak calls for each cell
#'   population passed on in the cell subsets argument. Each peak call is
#'   returned as as Genomic Ranges object.
#'
#' @details The technical details of the algorithm are found in XX.
#'
#' @references XX
#'
#' @noRd
#'

callTilesBySample <- function(blackList,
                              returnAllTiles = FALSE,
                              totalFrags,
                              fragsList,
                              cellCol = "RG",
                              verbose = FALSE,
                              StudypreFactor) {
  # Coefficients trained on ~ 3600 frags per cell
  # Future datasets need to be calibrated to
  # these coefficients

  if (any(is.na(fragsList))) {
    if (verbose) {
      warning("No cells from this sample. Returning NULL.")
    }
    return(NULL)
  }

  finalModelObject <- MOCHA::finalModelObject

  FinalBins <- determine_dynamic_range(
    AllFragmentsList = fragsList,
    blackList = blackList,
    binSize = 500,
    doBin = FALSE
  )
  if (length(FinalBins) == 0) {
    stop(
      "Could not bin fragments into 500bp tiles. ",
      "Verify that input fragments are not all in a blacklisted region."
    )
  }
  countsMatrix <- calculate_intensities(
    fragMat = fragsList,
    candidatePeaks = FinalBins,
    totalFrags = totalFrags,
    cellCol = cellCol,
    verbose = verbose
  )
  expected_names <- c(
    "tileID",
    "seqnames",
    "start",
    "end",
    "strand",
    "TotalIntensity",
    "maxIntensity",
    "numCells"
  )

  # Validate the countsMatrix from calculateIntensities
  if (!all(names(countsMatrix) == expected_names)) {
    stop("countsMatrix is missing required columns")
  }
  if (dim(countsMatrix)[1] == 0) {
    stop("countsMatrix is empty")
  }

  countsMatrix$TotalIntensity <- countsMatrix$TotalIntensity * StudypreFactor
  countsMatrix$maxIntensity <- countsMatrix$maxIntensity * StudypreFactor

  countsMatrix <- countsMatrix[countsMatrix$TotalIntensity > 0, ]
  MOCHA_tiles <- make_prediction(
    X = countsMatrix,
    finalModelObject = finalModelObject
  )

  if (!returnAllTiles) {
    MOCHA_tiles <- MOCHA_tiles[MOCHA_tiles$peak == T]
  }

  return(MOCHA_tiles)
}

#' @title \code{callTilesBySample}
#'
#' @description \code{callTilesBySample} is the main peak-calling function in MOCHA
#'   that serves as a wrapper function to call peaks provided a set of fragment
#'   files and cell metadata
#'
#' @param x a list of blackList, total fragment number, fragsList, verbose, study prefactors, etc..
#'
#' @return scMACs_PeakList an list containing peak calls for each cell
#'   population passed on in the cell subsets argument. Each peak call is
#'   returned as as Genomic Ranges object.
#'
#'
#' @noRd
#'
#'

simplifiedTilesBySample <- function(x) {
  callTilesBySample(
    blackList = x[[1]],
    returnAllTiles = TRUE,
    totalFrags = length(x[[2]]),
    fragsList = x[[2]],
    cellCol = x[[3]],
    verbose = x[[4]],
    StudypreFactor = x[[5]]
  )
}
