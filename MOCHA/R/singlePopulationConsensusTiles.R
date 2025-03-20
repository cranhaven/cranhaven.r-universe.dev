#' @title \code{singlePopulationConsensusTiles}
#'
#' @description \code{singlePopulationConsensusTiles} is an R helper function, part of
#'   the single-cell peak
#'
#' @keywords internal

singlePopulationConsensusTiles <- function(peaksExperiment,
                                           sampleData,
                                           threshold,
                                           groupColumn = NULL,
                                           verbose = TRUE) {
  if (threshold == 0) {
    disableMinimum <- TRUE
  } else {
    disableMinimum <- FALSE
  }

  # Extract matrices of samples by peak tileIDs with peak status
  # and set NA to FALSE (no peak here)
  samplePeakMat <- RaggedExperiment::compactAssay(
    peaksExperiment,
    i = "peak"
  )

  # Identify any samples that had less than 5 cells, and therefore no peak calls
  emptySamples <- apply(samplePeakMat, 2, function(x) all(is.na(x) | !x))

  # Now we'll filter out those samples, and replace all NAs with zeros.
  samplePeakMat <- samplePeakMat[, !emptySamples, drop = FALSE]
  samplePeakMat[is.na(samplePeakMat)] <- FALSE
  sampleData <- sampleData[rownames(sampleData) %in% names(which(!emptySamples)), , drop = FALSE]

  # The number of samples for reproducibility will be all samples with peak calls
  nSamples <- sum(!emptySamples)

  if (is.null(groupColumn)) {
    # Count the number of TRUE peaks in each row and divide by nSamples

    if (threshold <= 1 / nSamples & !disableMinimum) {
      if (verbose) {
        warning(
          "Threshold will result in only one sample needed to keep ",
          "a tile. Set threshold to 0 to ",
          "explicitly keep the union of tiles across all samples."
        )
      }
    }
    if (threshold == 0) {
      # Treat zero as the union
      threshold <- 1 / nSamples
    }

    percentTruePeaks <- rowSums(samplePeakMat) / nSamples

    # Keep only peaks with reproducibility above specified threshold
    consensusPeaks <- percentTruePeaks[percentTruePeaks >= threshold]
    consensusPeaks <- names(consensusPeaks)
  } else {
    # Get consensus peaks for groupings of samples
    groups <- unique(sampleData[[groupColumn]])
    consensusPeaksByGroup <- list()

    # We need to throw an error if there are too few samples for a given group to meet threshold
    if (any(threshold <= 1 / table(sampleData[[groupColumn]])) & !disableMinimum) {
      if (verbose) {
        warning(
          "Threshold will result in only one sample needed to keep",
          " a tile within one or more of your groups. Set threshold to 0 to ",
          "explicitly keep the union of tiles across all samples."
        )
      }
    }

    # The user may accidentally provide a groupColumn that includes NAs - which would mean that a set of Samples would be ignored entirely for peak calling.
    # We will throw this error and force the user to either exclude the samples before analysis, or edit the sampledata.
    if (any(is.na(sampleData[[groupColumn]]))) {
      stop("Error: GroupColumn contains NAs. If you want to call reproducible tiles within groups, provide a group annotation that includes all samples.")
    }

    for (group in groups) {
      # Filter sample-peak matrix to samples in this group
      samplesInGroupDF <- sampleData[sampleData[[groupColumn]] == group, ]
      samplesInGroup <- rownames(samplesInGroupDF)
      groupSamplePeakMat <- samplePeakMat[, samplesInGroup, drop = FALSE]

      if (threshold == 0) {
        # Treat zero as the union within this group
        threshold <- 1 / length(samplesInGroup)
      }

      # Keep only peaks with reproducibility above specified threshold
      percentTruePeaks <- rowSums(groupSamplePeakMat) / length(samplesInGroup)
      consensusPeaks <- percentTruePeaks[percentTruePeaks >= threshold]
      consensusPeaks <- names(consensusPeaks)

      consensusPeaksByGroup <- append(consensusPeaksByGroup, list(consensusPeaks))
    }
    names(consensusPeaksByGroup) <- groups

    consensusPeaks <- Reduce(union, consensusPeaksByGroup)
  }

  return(consensusPeaks)
}

#' @title \code{simplifiedConsensusTiles}
#' @param ref ref a list of data on one specific cell type, sampleData, threshold, groupColumn, and verbose flag.
#' @description \code{simplifiedConsensusTiles} is an R helper function, part of
#'   the single-cell peak
#'
#' @keywords internal
#'

simplifiedConsensusTiles <- function(ref) {
  experiments <- ref[[1]]
  sampleData <- ref[[2]]
  threshold <- ref[[3]]
  groupColumn <- ref[[4]]
  verbose <- ref[[5]]
  singlePopulationConsensusTiles(
    experiments,
    sampleData,
    threshold,
    groupColumn,
    verbose = verbose
  )
}

#' @title \code{extractErrorFromConsensusTiles}
#' @param x a specific output from consensus tiles.
#' @description \code{extractErrorFromConsensusTiles} is an R helper function, part of
#'   the single-cell peak calling.
#'
#' @keywords internal
#'


extractErrorFromConsensusTiles <- function(x) {
  if (any(grepl("Error", x))) {
    x[grep("Error", x)]
  } else {
    NA
  }
}
