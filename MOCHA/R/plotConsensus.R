#' @title \code{plotConsensus}
#'
#' @description \code{plotConsensus} Extracts the peak reproducibility and generates a
#' 					heuristic plots that can be used to determine the reproducibility threshold
#' 					used within getSampleTileMatrix.
#' @param tileObject A MultiAssayExperiment object from callOpenTiles,
#' @param cellPopulations the cell populations you want to visualize.
#' @param groupColumn Optional parameter, same as in getSampleTileMatrix, which defines whether you
#' 				  want to plot reproducibility within each
#' @param returnPlotList Instead of one plot with all celltypes/conditions, it returns a list of plots for each cell types
#' @param returnDFs Instead of a plot, returns a data.frame of the reproducibility across samples.
#' 						If set to false, then it plots the data.frame instead of returning it.
#' @param numCores Number of cores to multithread over.
#'
#' @return SampleTileObj the input data structure with added gene annotations.
#'
#'
#' @export

plotConsensus <- function(tileObject,
                          cellPopulations = "All",
                          groupColumn = NULL,
                          returnPlotList = FALSE,
                          returnDFs = FALSE,
                          numCores = 1) {
  Reproducibility <- PeakNumber <- groups <- GroupName <- NULL

  if (all(tolower(cellPopulations) == "all")) {
    subTileResults <- tileObject
    cellPopulations <- names(tileObject)
  } else {
    if (all(cellPopulations %in% names(tileObject))) {
      subTileResults <- tileObject[names(tileObject) %in% cellPopulations]
    } else {
      stop(paste(
        "All of `cellPopulations` must present in tileResults.",
        "Check `names(tileResults)` for possible cell populations."
      ))
    }
  }

  sampleData <- SummarizedExperiment::colData(tileObject)


  iterList <- lapply(names(subTileResults), function(x) {
    list(subTileResults[[x]], sampleData, groupColumn, returnPlotList)
  })

  # return(iterList)
  # cl <- parallel::makeCluster(numCores)

  alldf <- pbapply::pblapply(cl = numCores, X = iterList, cellTypeDF)

  names(alldf) <- names(subTileResults)

  if (returnDFs) {
    return(alldf)
  }

  if (returnPlotList) {
    if (!is.null(groupColumn)) {
      allPlots2 <- lapply(seq_along(alldf), function(x) {
        ggplot2::ggplot(alldf[[x]], ggplot2::aes(x = Reproducibility, y = PeakNumber, group = GroupName, color = GroupName)) +
          ggplot2::geom_point() +
          ggplot2::ggtitle(names(alldf)[x]) +
          ggplot2::scale_y_continuous(trans = "log2") +
          ggplot2::ylab("Peak Number") +
          ggplot2::theme_bw()
      })

      return(allPlots2)
    } else {
      allPlots <- lapply(seq_along(alldf), function(x) {
        ggplot2::ggplot(alldf[[x]], ggplot2::aes(x = Reproducibility, y = PeakNumber)) +
          ggplot2::geom_point() +
          ggplot2::ggtitle(names(alldf)[x]) +
          ggplot2::scale_y_continuous(trans = "log2") +
          ggplot2::ylab("Peak Number") +
          ggplot2::theme_bw()
      })
      return(allPlots)
    }
  } else {
    combinedDF <- do.call("rbind", alldf)
    combinedDF$CellPop <- gsub("\\.", "", gsub("[0-9]{1,3}", "", rownames(combinedDF)))

    if (!is.null(groupColumn)) {
      combinedDF$groups <- paste(combinedDF$CellPop, combinedDF$GroupName, sep = "_")
    } else {
      combinedDF$groups <- combinedDF$CellPop
    }

    p1 <- ggplot2::ggplot(combinedDF, ggplot2::aes(x = Reproducibility, y = PeakNumber, group = groups, color = groups)) +
      ggplot2::geom_line() +
      ggplot2::scale_y_continuous(trans = "log10") +
      ggplot2::ylab("Peak Number") +
      ggplot2::theme_bw()

    return(p1)
  }
}


cellTypeDF <- function(list1 = NULL, peaksExperiment, sampleData, groupColumn, returnPlotList = FALSE) {
  if (!is.null(list1)) {
    peaksExperiment <- list1[[1]]
    sampleData <- list1[[2]]
    groupColumn <- list1[[3]]
    returnPlotList <- list1[[4]]
  }
  samplePeakMat <- RaggedExperiment::compactAssay(
    peaksExperiment,
    i = "peak"
  )

  # Identify any samples that had less than 5 cells, and therefore no peak calls
  emptySamples <- apply(samplePeakMat, 2, function(x) all(is.na(x) | !x))

  # Now we'll filter out those samples, and replace all NAs with zeros.
  samplePeakMat <- samplePeakMat[, !emptySamples]
  samplePeakMat[is.na(samplePeakMat)] <- FALSE
  sampleData <- sampleData[rownames(sampleData) %in% names(which(!emptySamples)), ]



  if (is.null(groupColumn)) {

    # The number of samples for reproducibility will be all samples with peak calls
    nSamples <- sum(!emptySamples)
    reproducibility_perc <- seq(0, 1, by = 1 / nSamples)

    # Count the number of TRUE peaks in each row and divide by nSamples
    peakReps <- rowSums(samplePeakMat) / nSamples
    RepPeaks <- sapply(reproducibility_perc, function(x) sum(peakReps >= x))
    TruePeaks <- data.frame("Reproducibility" = reproducibility_perc, "PeakNumber" = RepPeaks)
  } else {

    # Get consensus peaks for groupings of samples
    groups <- unique(sampleData[[groupColumn]])
    TruePeaks <- data.frame()
    for (group in groups) {
      # Filter sample-peak matrix to samples in this group
      samplesInGroupDF <- sampleData[sampleData[[groupColumn]] == group, ]
      samplesInGroup <- rownames(samplesInGroupDF)
      groupSamplePeakMat <- samplePeakMat[, samplesInGroup]

      reproducibility_perc <- seq(0, 1, by = 1 / length(samplesInGroup))

      peakReps_tmp <- rowSums(groupSamplePeakMat) / length(samplesInGroup)
      RepPeaks_tmp <- sapply(reproducibility_perc, function(x) sum(peakReps_tmp >= x))
      TruePeaks_tmp <- data.frame("Reproducibility" = reproducibility_perc, "PeakNumber" = RepPeaks_tmp)

      TruePeaks_tmp$GroupName <- rep(group, length(RepPeaks_tmp))

      TruePeaks <- rbind(TruePeaks, TruePeaks_tmp)
    }
  }

  return(TruePeaks)
}
