#' @title \code{extractRegion}
#'
#' @description \code{extractRegion} will extract the coverage files created by
#'   callOpenTiles and return a specific region's coverage
#'
#' @param SampleTileObj The SummarizedExperiment object output from
#'   getSampleTileMatrix
#' @param type Boolean. Default is true, and exports Coverage. If set to FALSE,
#'   exports Insertions.
#' @param region a GRanges object or vector or strings containing the regions of
#'   interest. Strings must be in the format "chr:start-end", e.g.
#'   "chr4:1300-2222".
#' @param cellPopulations vector of strings. Cell subsets for which to call
#'   peaks. This list of group names must be identical to names that appear in
#'   the SampleTileObj.  Optional, if cellPopulations='ALL', then peak calling
#'   is done on all cell populations. Default is 'ALL'.
#' @param groupColumn Optional, the column containing sample group labels for
#'   returning coverage within sample groups. Default is NULL, all samples will
#'   be used.
#' @param subGroups a list of subgroup(s) within the groupColumn from the
#'   metadata. Optional, default is NULL, all labels within groupColumn will be
#'   used.
#' @param sampleSpecific If TRUE, get a sample-specific count dataframe out.
#'   Default is FALSE, average across samples and get a dataframe out.
#' @param approxLimit Optional limit to region size, where if region is larger
#'   than approxLimit basepairs, binning will be used. Default is 100000.
#' @param binSize Optional numeric, size of bins in basepairs when binning is
#'   used. Default is 250.
#' @param sliding Optional numeric. Default is NULL. This number is the size of
#'   the sliding window for generating average intensities.
#' @param numCores integer. Number of cores to parallelize peak-calling across
#'   multiple cell populations
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return countSE a SummarizedExperiment containing coverage for the given
#'   input cell populations.
#'
#' @examples
#' \dontrun{
#' countSE <- MOCHA::extractRegion(
#'   SampleTileObj = SampleTileMatrices,
#'   cellPopulations = "ALL",
#'   region = "chr1:18137866-38139912",
#'   numCores = 30,
#'   sampleSpecific = FALSE
#' )
#' }
#'
#' @export
#'

extractRegion <- function(SampleTileObj,
                          type = TRUE,
                          region,
                          cellPopulations = "ALL",
                          groupColumn = NULL,
                          subGroups = NULL,
                          sampleSpecific = FALSE,
                          approxLimit = 100000,
                          binSize = 250,
                          sliding = NULL,
                          numCores = 1,
                          verbose = FALSE) {
  . <- idx <- score <- NULL

  cellNames <- names(SummarizedExperiment::assays(SampleTileObj))
  metaFile <- SummarizedExperiment::colData(SampleTileObj)
  outDir <- SampleTileObj@metadata$Directory

  if (is.na(outDir)) {
    stop("Missing coverage file directory. SampleTileObj$metadata must contain 'Directory'.")
  }

  if (!file.exists(outDir)) {
    stop("Directory given by SampleTileObj@metadata$Directory does not exist.")
  }

  if (is.character(region)) {

    # Convert to GRanges
    regionGRanges <- MOCHA::StringsToGRanges(region)
  } else if (class(region)[1] == "GRanges") {
    regionGRanges <- region
  } else {
    stop("Wrong region input type. Input must either be a string, or a GRanges location.")
  }

    
    
  if (all(toupper(cellNames) == "COUNTS")) {
    stop(
      "The only assay in the SummarizedExperiment is Counts. The names of assays must reflect cell types,",
      " such as those in the Summarized Experiment output of getSampleTileMatrix."
    )
  }

  if (all(toupper(cellPopulations) == "ALL")) {
    cellPopulations <- cellNames
  }
  if (!all(cellPopulations %in% cellNames)) {
    stop("Some or all cell populations provided are not found.")
  }

  # Pull out a list of samples by group.

  if (!is.null(subGroups) & !is.null(groupColumn)) {
    # If the user defined a list of subgroup(s) within the groupColumn from the metadata, then it subsets to just those samples
    subSamples <- lapply(subGroups, function(x) metaFile[metaFile[, groupColumn] %in% x, "Sample"])
    names(subSamples) <- subGroups
  } else if (!is.null(groupColumn)) {

    # If no subGroup defined, then it'll form a list of samples across all labels within the groupColumn
    subGroups <- unique(metaFile[, groupColumn])

    subSamples <- lapply(subGroups, function(x) metaFile[metaFile[, groupColumn] %in% x, "Sample"])
  } else {

    # If neither groupColumn nor subGroup is defined, then it forms one list of all sample names
    subGroups <- "All"
    subSamples <- list("All" = metaFile[, "Sample"])
  }

  # Determine if binning is needed to simplify things
  if (GenomicRanges::end(regionGRanges) - GenomicRanges::start(regionGRanges) > approxLimit) {
    if (verbose) {
      message(stringr::str_interp("Size of region exceeds ${approxLimit}bp. Binning data over ${binSize}bp windows."))
    }
    if (is.null(sliding)) {
      binnedData <- regionGRanges %>%
        plyranges::tile_ranges(., binSize) %>%
        dplyr::mutate(idx = c(1:length(.)))
    } else if (is.numeric(sliding)) {
      binnedData <- regionGRanges %>%
        plyranges::slide_ranges(., width = binSize, step = sliding) %>%
        dplyr::mutate(idx = c(1:length(.)))
    }
  } else if (!is.null(sliding)) {
    stop("The sliding average is completely ignored, because the approxLimit is larger than than the window size. Please change approxLimit if you want to use a sliding average.")
  }

  cl <- parallel::makeCluster(numCores)
  # Pull up the cell types of interest, and filter for samples and subset down to region of interest
  cellPopulation_Files <- lapply(cellPopulations, function(x) {

    # Pull up coverage files
    originalCovGRanges <- readRDS(paste(outDir, "/", x, "_CoverageFiles.RDS", sep = ""))
    if(type & 'Accessibility' %in% names(originalCovGRanges)){
     originalCovGRanges <- originalCovGRanges[['Accessibility']]
    }else if (type & !'Accessibility' %in% names(originalCovGRanges)){
      originalCovGRanges <- originalCovGRanges
    }else if (!type & 'Accessibility' %in% names(originalCovGRanges)){
      originalCovGRanges <- originalCovGRanges[['Insertions']]
    }else{
        stop('Error around reading coverage files. Check that coverage files are not corrupted.')
    }
    
    
    # Edge case: One or more samples are missing coverage for this cell population,
    # e.g. if a cell population only exists in one sample.
    lapply(seq_along(subSamples), function(y) {
      if (!all(subSamples[[y]] %in% names(originalCovGRanges))) {
        missingSamples <- paste(subSamples[[y]][!subSamples[[y]] %in% names(originalCovGRanges)], collapse = ", ")
        stop(stringr::str_interp(c(
          "There is no fragment coverage for cell population '${x}' in the ",
          "following samples in sample grouping '${names(subSamples)[y]}': ",
          "${missingSamples}"
        )))
      }
    })

    if (verbose) {
      message(stringr::str_interp("Extracting coverage from cell population '${x}'"))
    }

    # If the region is too large, bin the data.
    if (GenomicRanges::end(regionGRanges) - GenomicRanges::start(regionGRanges) > approxLimit) {
      iterList <- lapply(seq_along(subSamples), function(y) {
        list(binnedData, originalCovGRanges[subSamples[[y]]])
      })

      # If not sample specific, take the average coverage across samples.
      # if it is sample specific, just subset down the coverage to the region of interest.
      if (!sampleSpecific) {
        cellPopSubsampleCov <- pbapply::pblapply(cl = cl, X = iterList, averageBinCoverage)
      } else {
        cellPopSubsampleCov <- pbapply::pblapply(cl = cl, X = iterList, subsetBinCoverage)
      }
    } else {
      iterList <- lapply(seq_along(subSamples), function(y) {
        list(regionGRanges, originalCovGRanges[subSamples[[y]]])
      })

      # If not sample specific, take the average coverage across samples.
      # if it is sample specific, just subset down the coverage to the region of interest.
      if (!sampleSpecific) {
        cellPopSubsampleCov <- pbapply::pblapply(cl = cl, X = iterList, averageBPCoverage)
      } else {
        cellPopSubsampleCov <- pbapply::pblapply(cl = cl, X = iterList, subsetBPCoverage)
      }
    }

    names(cellPopSubsampleCov) <- subGroups
    cellPopSubsampleCov
  })

  names(cellPopulation_Files) <- cellPopulations
  allGroups <- unlist(cellPopulation_Files)

  if (all(lengths(allGroups) == 0)) {
    stop("No fragments found for the region provided.")
  }

  ## Generate a data.frame for export.
  iterList <- lapply(seq_along(allGroups), function(x) {
    list(allGroups[[x]], names(allGroups)[x])
  })

  if (GenomicRanges::end(regionGRanges) - GenomicRanges::start(regionGRanges) > approxLimit) {
    allGroupsDF <- pbapply::pblapply(cl = cl, iterList, cleanDataFrame1)
  } else {
    allGroupsDF <- pbapply::pblapply(cl = cl, iterList, cleanDataFrame2)
  }
  parallel::stopCluster(cl)

  names(allGroupsDF) <- names(allGroups)

  newMetadata <- SampleTileObj@metadata
  newMetadata$History <- append(newMetadata$History, paste("extractRegion", utils::packageVersion("MOCHA")))
  if (type) {
    Type1 <- "Coverage"
  } else {
    Type1 <- "Insertions"
  }
  newMetadata$Type <- Type1

  countSE <- SummarizedExperiment::SummarizedExperiment(allGroupsDF,
    metadata = newMetadata
  )

  return(countSE)
}


## helper functions.
                         
# Generates average single basepair coverage for a given region
averageBPCoverage <- function(iterList) {
  regionGRanges <- iterList[[1]]
  sampleCount <- length(iterList[[2]])

  filterCounts <- lapply(1:sampleCount, function(z) {
    plyranges::join_overlap_intersect(iterList[[2]][[z]], regionGRanges)
  })

  mergedCounts <- IRanges::stack(methods::as(filterCounts, "GRangesList"))
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges)
  mergedCounts <- plyranges::compute_coverage(mergedCounts, weight = mergedCounts$score / sampleCount)
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges)

  return(mergedCounts)
}

## Efficiently subsets single basepair coverage for a given region across samples
subsetBPCoverage <- function(iterList) {
  sampleCount <- length(iterList[[2]])

  filterCounts <- lapply(1:sampleCount, function(z) {
    plyranges::join_overlap_intersect(iterList[[2]][[z]], iterList[[1]])
  })

  mergedCounts <- IRanges::stack(methods::as(filterCounts, "GRangesList"))
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, iterList[[1]])
  mergedCounts <- plyranges::compute_coverage(mergedCounts, weight = mergedCounts$score / sampleCount)
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, iterList[[1]])

  return(mergedCounts)
}

# Generates average single basepair coverage for a given region
averageBPCoverage <- function(iterList) {
  regionGRanges <- iterList[[1]]
  sampleCount <- length(iterList[[2]])
  
  filterCounts <- lapply(1:sampleCount, function(z) {
    plyranges::join_overlap_intersect(iterList[[2]][[z]], regionGRanges)
  })
  
  mergedCounts <- IRanges::stack(methods::as(filterCounts, "GRangesList"))
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges)
  mergedCounts <- plyranges::compute_coverage(mergedCounts, weight = mergedCounts$score / sampleCount)
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges)
  
  return(mergedCounts)
}

## Efficiently subsets and bins coverage for a given region across samples
subsetBinCoverage <- function(iterList) {
  partition <- idx <- score <- NULL
  regionGRanges_tmp <- plyranges::reduce_ranges(iterList[[1]])
  indTiles <- plyranges::select(plyranges::tile_ranges(regionGRanges_tmp, 1), -partition)

  tmpCounts <- lapply(iterList[[2]], function(z) {
    tmpGR <- plyranges::join_overlap_intersect(z, indTiles)
    tmpGR <- plyranges::join_overlap_intersect(tmpGR, iterList[[1]])
    tmpGR <- plyranges::group_by(tmpGR, idx)
    tmpGR <- plyranges::reduce_ranges(tmpGR, score = mean(score))
    tmpGR <- dplyr::ungroup(tmpGR)
    tmpGR
  })

  return(tmpCounts)
}


## Efficiently generates averaged coverage across samples by bins within a given region
averageBinCoverage <- function(iterList) {
  idx <- score <- partition <- NULL
  regionGRanges_tmp <- plyranges::reduce_ranges(iterList[[1]])
  indTiles <- plyranges::select(plyranges::tile_ranges(regionGRanges_tmp, 1), -partition)
  sampleCount <- length(iterList[[2]])

  filterCounts <- lapply(iterList[[2]], function(z) {
    tmpGR <- plyranges::join_overlap_intersect(z, indTiles)
    tmpGR <- plyranges::join_overlap_intersect(tmpGR, iterList[[1]])
    tmpGR <- plyranges::group_by(tmpGR, idx)
    tmpGR <- plyranges::reduce_ranges(tmpGR, score = mean(score))
    tmpGR <- dplyr::ungroup(tmpGR)
    tmpGR
  })

  mergedCounts <- IRanges::stack(methods::as(filterCounts, "GRangesList"))
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges_tmp)
  mergedCounts <- plyranges::compute_coverage(mergedCounts, weight = mergedCounts$score / sampleCount)
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges_tmp)
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, iterList[[1]])


  return(mergedCounts)

}

# This function is not used.
## Efficiently subsets and bins coverage for a given region across samples
# subsetSlidingBinCoverage <- function(iterList) {
#   idx <- NULL
#   binnedData <- iterList[[1]]
#   regionGRanges <- plyranges::reduce_ranges(binnedData)
# 
#   tmpCounts <- lapply(subList, function(z) {
#     tmpGR <- plyranges::join_overlap_intersect(z, regionGRanges) %>%
#       tmpGR() <- plyranges::join_overlap_intersect(tmpGR, binnedData)
#     tmpGR <- plyranges::group_by(tmpGR, idx) %>%
#       tmpGR() <- plyranges::reduce_ranges(tmpGR, score = mean(score))
#     tmpGR <- dplyr::ungroup(tmpGR)
#     tmpGR
#   })
# 
#   return(tmpCounts)
# }


## Efficiently generates averaged coverage across samples by bins within a given region
averageSlidingBinCoverage <- function(iterList) {
  regionGRanges <- plyranges::reduce_ranges(iterList[[1]])
  sampleCount <- length(iterList[[2]])

  filterCounts <- lapply(1:sampleCount, function(z) {
    plyranges::join_overlap_intersect(iterList[[2]][[z]], regionGRanges)
  })

  mergedCounts <- IRanges::stack(methods::as(filterCounts, "GRangesList"))
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges)
  mergedCounts <- plyranges::compute_coverage(mergedCounts, weight = mergedCounts$score / sampleCount)
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, regionGRanges)
  mergedCounts <- plyranges::join_overlap_intersect(mergedCounts, iterList[[1]])


  return(mergedCounts)
}


cleanDataFrame1 <- function(iterList) {
  group1 <- iterList[[1]]
  subGroupdf <- as.data.frame(group1)
  subGroupdf$Groups <- rep(iterList[[2]], length(group1))

  covdf <- subGroupdf[, c("seqnames", "start", "score", "Groups")]
  colnames(covdf) <- c("chr", "Locus", "Counts", "Groups")

  return(covdf)
}

cleanDataFrame2 <- function(iterList) {
  group1 <- iterList[[1]]
  tmp <- plyranges::tile_ranges(group1, width = 1)
  tmp$score <- group1$score[tmp$partition]
  tmp$Groups <- rep(iterList[[2]], length(tmp))


  covdf <- as.data.frame(tmp)[, c("seqnames", "start", "score", "Groups")]
  colnames(covdf) <- c("chr", "Locus", "Counts", "Groups")

  return(covdf)
}
