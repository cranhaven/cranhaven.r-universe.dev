#' @title \code{exportCoverage}
#'
#' @description \code{exportCoverage} will export normalized coverage files to
#'   BigWig files, either as sample-specific or sample-averaged files, for
#'   visualization in genome browsers.
#'
#' @param SampleTileObject The SummarizedExperiment object output from
#'   getSampleTileMatrix
#' @param dir string. Directory to save files to.
#' @param cellPopulations vector of strings. Cell subsets for which to call
#'   peaks. This list of group names must be identical to names that appear in
#'   the SampleTileObject.  Optional, if cellPopulations='ALL', then peak calling
#'   is done on all cell populations. Default is 'ALL'.
#' @param type Boolean. Default is TRUE, and exports Coverage. If set to FALSE,
#'   exports Insertions.
#' @param groupColumn Optional, the column containing sample group labels for
#'   returning coverage within sample groups. Default is NULL, all samples will
#'   be used.
#' @param subGroups a list of subgroup(s) within the groupColumn from the
#'   metadata. Optional, default is NULL, all labels within groupColumn will be
#'   used.
#' @param sampleSpecific If TRUE, a BigWig will export for each sample-cell type
#'   combination.
#' @param saveFile Boolean. If TRUE, it will save to a BigWig. If FALSE, it will
#'   return the GRangesList without writing a BigWig.
#' @param numCores integer. Number of cores to parallelize peak-calling across
#'   multiple cell populations
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return countSE a SummarizedExperiment containing coverage for the given
#'   input cell populations.
#'
#' @examples
#' \dontrun{
#' MOCHA::exportCoverage(
#'   SampleTileObject = SampleTileMatrices,
#'   cellPopulations = "ALL",
#'   numCores = 30,
#'   sampleSpecific = FALSE
#' )
#' }
#'
#' @export
#'

exportCoverage <- function(SampleTileObject,
                           dir = getwd(),
                           type = TRUE,
                           cellPopulations = "ALL",
                           groupColumn = NULL,
                           subGroups = NULL,
                           sampleSpecific = FALSE,
                           saveFile = TRUE,
                           numCores = 1,
                           verbose = FALSE) {
  . <- idx <- score <- NULL

  cellNames <- names(SummarizedExperiment::assays(SampleTileObject))
  metaFile <- SummarizedExperiment::colData(SampleTileObject)
  outDir <- SampleTileObject@metadata$Directory

  if (is.na(outDir)) {
    stop("Missing coverage file directory. SampleTileObject$metadata must contain 'Directory'.")
  }

  if (!file.exists(outDir)) {
    stop("Directory given by SampleTileObject@metadata$Directory does not exist.")
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


  cl <- parallel::makeCluster(numCores)
  parallel::clusterEvalQ(cl, {
    library(GenomicRanges)
  })
  # Pull up the cell types of interest, and filter for samples and subset down to region of interest
  allCellPopCoverage <- NULL
  for (x in cellPopulations) {
    # MOCHA::getCoverage outputs a single list with two named items: "Accessibility"
    # and "Insertions".
    # This is saved to *_CoverageFiles.RDS in MOCHA::callOpenTiles
    if (type) { # Accessibility
      originalCovGRanges <- readRDS(paste(outDir, "/", x, "_CoverageFiles.RDS", sep = ""))
      # For backwards compatibility, only use "Accessibility" if it exists.
      if ("Accessibility" %in% names(originalCovGRanges)) {
        originalCovGRanges <- originalCovGRanges$Accessibility
      }
    } else { # Insertions
      originalCovGRanges <- readRDS(paste(outDir, "/", x, "_CoverageFiles.RDS", sep = ""))
      # For backwards compatibility, only use "Insertions" if it exists.
      if ("Insertions" %in% names(originalCovGRanges)) {
        originalCovGRanges <- originalCovGRanges$Insertions
      } else {
        # Check for a separate "Insertions" file
        originalCovGRanges <- readRDS(paste(outDir, "/", x, "_InsertionFiles.RDS", sep = ""))
      }
    }

    if (verbose) {
      message(stringr::str_interp("Extracting coverage for cell population ${x}."))
    }

    iterList <- lapply(subSamples, function(y) {
      originalCovGRanges[y]
    })
    
    # Check for and remove null elements from iterlist
    # Covers edge case where coverage files do not contain all samples
    missingSamples <- lapply(seq_along(iterList), function(x) {
      groupIterList <- iterList[[x]]
      groupSubSamples <- subSamples[[x]]
      groupSubSamples[lengths(groupIterList) == 0]
    })
    missingSamples <- unique(unlist(missingSamples))
    
    if (length(missingSamples) > 0) {
      if(verbose) { 
        warning(
          "The following samples have no saved coverage and will be skipped: ",
          paste0(missingSamples, collapse = ", ")
        ) 
      }
      iterList <- lapply(iterList, function(x) {
        x[lengths(x) != 0]
      })
    }
    
    # If not sample specific, take the average coverage across samples.
    # if it is sample specific, just subset down the coverage to the region of interest.
    if (!sampleSpecific) {
      cellPopSubsampleCov <- pbapply::pblapply(cl = cl, X = iterList, averageCoverage)
      names(cellPopSubsampleCov) <- subGroups
    } else { # sample specific
      names(iterList) <- subGroups
      cellPopSubsampleCov <- unlist(iterList, recursive = FALSE)
      names(cellPopSubsampleCov) <- gsub("\\.", "__", names(cellPopSubsampleCov))
    }

    if (saveFile) {
      # Export bigwig
      for (i in 1:length(cellPopSubsampleCov)) {
        fileName <- gsub(" ", "__", paste(x, groupColumn, names(cellPopSubsampleCov)[i], sep = "__"))
        if (!type) {
          fileName <- paste(fileName, "__Insertions", sep = "")
        }
        # Edge case where there was only 1 sample to be averaged if !sampleSpecific
        if (methods::is(cellPopSubsampleCov[[i]], "list")){
          if (length(cellPopSubsampleCov[[i]]) == 1) {
            cellPopSubsampleCov[[i]] <- cellPopSubsampleCov[[i]][[1]]
          }
        }
        if (verbose) {
          message("Exporting: ", paste(dir, "/", fileName, ".bw", sep = ""))
        }
        plyranges::write_bigwig(cellPopSubsampleCov[[i]], paste(dir, "/", fileName, ".bw", sep = ""))
      }
    }

    names(cellPopSubsampleCov) <- x
    allCellPopCoverage <- append(allCellPopCoverage, cellPopSubsampleCov)

  }

  return(allCellPopCoverage)
}


## helper function
## Efficiently generates average single basepair coverage for a given region
averageCoverage <- function(coverageList) {
  sampleCount <- length(coverageList)
  if (sampleCount > 1) {
    mergedCounts <- IRanges::stack(methods::as(coverageList, "GRangesList"))
    mergedCounts <- plyranges::compute_coverage(mergedCounts, weight = mergedCounts$score / sampleCount)
  } else {
    mergedCounts <- coverageList
  }


  return(mergedCounts)
}


#' @title \code{exportDifferentials}
#'
#' @description \code{exportDifferentials} exports the differential peaks
#'  output GRangesList output from \code{getDifferentialAccessibleTiles} to
#'  bigBed format for visualization in genome browsers.
#'
#' @param SampleTileObject The SummarizedExperiment object output from
#'   \code{getSampleTileMatrix}
#' @param DifferentialsGRList GRangesList output from
#'   \code{getDifferentialAccessibleTiles}
#' @param outDir Desired output directory where bigBed files will be saved
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return outList A List of output filepaths
#' 
#' @examples
#' \dontrun{
#' MOCHA::exportDifferentials(
#'   SampleTileObject = SampleTileMatrices,
#'   DifferentialsGRList,
#'   outDir = tempdir(),
#'   verbose = TRUE
#' )
#' }
#'
#' @export
#'
exportDifferentials <- function(SampleTileObject,
                                DifferentialsGRList,
                                outDir,
                                verbose = FALSE) {

  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    stop(
      "Package 'rtracklayer' is required for exportDifferentials. ",
      "Please install 'rtracklayer' to proceed."
    )
  }
  genome <- BSgenome::getBSgenome(S4Vectors::metadata(SampleTileObject)$Genome)
  outList <- list()

  for (i in seq_along(DifferentialsGRList)) {
    comparison_name <- names(DifferentialsGRList)[[i]]
    if (is.null(comparison_name)) {
      comparison_name <- "Differentials"
    }
    DiffPeaksGR <- DifferentialsGRList[[i]]
    
    # Remove NA metadata
    # causes error: In isSingleString(path) : Unknown type 'NA'
    GenomicRanges::mcols(DiffPeaksGR) <- NULL
    
    # Set score and seqinfo for bigBed
    DiffPeaksGR$score <- 1

    GenomicRanges::seqinfo(DiffPeaksGR) <- GenomicRanges::seqinfo(genome)[GenomicRanges::seqnames(GenomicRanges::seqinfo(DiffPeaksGR))]

    outFile <- file.path(outDir, paste(comparison_name, sep = "__"))
    outFile <- paste0(outFile, ".bigBed")
    if (verbose) {
      message("Exporting: ", outFile)
    }
    
    # Output to bigbed
    rtracklayer::export.bb(GenomicRanges::GRanges(DiffPeaksGR), outFile)
    
    # Check for success
    if (!file.exists(outFile)) {
      stop("Silent error in rtracklayer::export.bb")
    }
    outList <- append(outList, outFile)
  }
  outList
}

#' @title \code{exportOpenTiles}
#'
#' @description \code{exportOpenTiles} exports the open tiles of a given cell
#'  population to bigBed file for visualization in genome browsers.
#'
#' @param SampleTileObject The SummarizedExperiment object output from
#'   \code{getSampleTileMatrix}
#' @param cellPopulation The name of the cell population to export
#' @param outDir Desired output directory where bigBed files will be saved
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return outList A List of output filepaths
#' 
#' @examples
#' \dontrun{
#' MOCHA::exportOpenTiles(
#'   SampleTileObject = SampleTileObject,
#'   cellPopulation,
#'   outDir = tempdir(),
#'   verbose = TRUE
#' )
#' }
#'
#' @export
#'
exportOpenTiles <- function(SampleTileObject,
                            cellPopulation,
                            outDir,
                            verbose = FALSE) {

  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    stop(
      "Package 'rtracklayer' is required for exportOpenTiles. ",
      "Please install 'rtracklayer' to proceed."
    )
  }
  genome <- BSgenome::getBSgenome(S4Vectors::metadata(SampleTileObject)$Genome)

  outList <- list()

  for (cellPopulation in names(SummarizedExperiment::assays(SampleTileObject))) {
    cellPopMatrix <- MOCHA::getCellPopMatrix(
      SampleTileObject,
      cellPopulation,
      NAtoZero = FALSE
    )
    for (sample in colnames(SampleTileObject)) {
      samplePeaks <- cellPopMatrix[,
        colnames(cellPopMatrix) %in% c(sample),
        drop = FALSE
      ]
      samplePeaksGR <- StringsToGRanges(rownames(samplePeaks))

      # Set score and seqinfo for bigBed
      samplePeaksGR$score <- 1

      GenomicRanges::seqinfo(samplePeaksGR) <- GenomicRanges::seqinfo(genome)[GenomicRanges::seqnames(GenomicRanges::seqinfo(samplePeaksGR))]


      sampleRow <- SummarizedExperiment::colData(SampleTileObject)[sample, ]
      pbmc_sample_id <- sampleRow[["Sample"]] # Enforced colname in callOpenTiles
      outFile <- file.path(outDir, paste(cellPopulation, pbmc_sample_id, sep = "__"))
      outFile <- paste0(outFile, ".bigBed")
      if (verbose) {
        message("Exporting: ", outFile)
      }

      # Output to bigbed
      rtracklayer::export.bb(samplePeaksGR, outFile)
      
      outList <- append(outList, outFile)
    }
  }
  outList
}

#' @title \code{exportMotifs}
#'
#' @description \code{exportMotifs} exports a motif set GRanges from running
#'    \code{addMotifSet(returnSTM=FALSE)} to bigBed file files for visualization
#'    in genome browsers.
#'    
#' @param SampleTileObject The SummarizedExperiment object output from
#'   \code{getSampleTileMatrix}
#' @param motifsGRanges A GRanges containing motif annotations, typically from
#'   \code{addMotifSet(returnSTM=FALSE)}
#' @param filterByOpenTiles Boolean. If TRUE, a bigBed file will be exported
#'   for each cell population with motifs filtered to those occurring
#'   only in open tiles.
#' @param outDir Desired output directory where bigBed files will be saved
#' @param motifSetName Optional, a name indicating the motif set. Used to name
#'   files in the specified \code{outdir}. Default is "motifs".
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' 
#' @return outList A List of output filepaths
#' 
#' @examples
#' \dontrun{
#' MOCHA::exportMotifs(
#'   SampleTileObject = SampleTileMatrices,
#'   motifsGRanges,
#'   motifSetName = "CISBP",
#'   filterByOpenTiles = FALSE,
#'   outDir = tempdir(),
#'   verbose = TRUE
#' )
#' }
#'
#' @export
#'
exportMotifs <- function(SampleTileObject,
                         motifsGRanges,
                         motifSetName = "motifs",
                         filterByOpenTiles = FALSE,
                         outDir,
                         verbose = FALSE) {
  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    stop(
      "Package 'rtracklayer' is required for exportMotifs. ",
      "Please install 'rtracklayer' to proceed."
    )
  }
  
  if (!methods::is(motifsGRanges, "GRanges")) {
    if (methods::is(motifsGRanges, "CompressedGRangesList")) {
      motifsGRanges <- unlist(motifsGRanges)
    } else {
      stop("`motifsGRanges` is an unrecognized type. `motifsGRanges` must
           be either 'GRanges' or 'CompressedGRangesList'.")
    }
  }
  
  # Map over the seqinfo from our genome to the motifsGRanges

  # Required for bigBed export 
  genome <- BSgenome::getBSgenome(S4Vectors::metadata(SampleTileObject)$Genome)
  GenomicRanges::seqinfo(motifsGRanges) <- GenomicRanges::seqinfo(genome)[GenomicRanges::seqnames(GenomicRanges::seqinfo(motifsGRanges))]
  
  # # Truncate trailing zeroes from score https://www.biostars.org/p/235193/
  # # (Error : Trailing characters parsing integer in field 4 line 1 of text, got 10.3405262378482)
  motifsGRanges$score <- floor(motifsGRanges$score)
  
  # Filter out negative scores https://github.com/jhkorhonen/MOODS/issues/12
  motifsGRanges <- motifsGRanges[motifsGRanges$score > 0]

  outList <- list()
  if (filterByOpenTiles) {
    # Split motifsGRangesFiltered by cell population, filtered to open tiles in cell population
    allPeaks <- SummarizedExperiment::rowRanges(SampleTileObject)
    samplePeakTable <- GenomicRanges::mcols(allPeaks)
    for (celltype in names(samplePeakTable)) {
      # Filter rows with boolean index
      samplePeaksGR <- allPeaks[samplePeakTable[[celltype]], ]
      cellTypePeakMotifs <- plyranges::filter_by_overlaps(motifsGRanges, samplePeaksGR)

      outFile <- file.path(outDir, paste(celltype, motifSetName, "motifset.bigBed", sep = "__"))
      if (verbose) {
        message("Exporting: ", outFile)
      }
      # Output to bigbed
      rtracklayer::export.bb(cellTypePeakMotifs, outFile)
      outList <- append(outList, outFile)
    }
  } else {
    outFile <- file.path(outDir, paste(motifSetName, "motifset.bigBed", sep = "__"))
    if (verbose) {
      message("Exporting: ", outFile)
    }
    # Output to bigbed
    rtracklayer::export.bb(motifsGRanges, outFile)
    outList <- append(outList, outFile)
  }
  outList
}
