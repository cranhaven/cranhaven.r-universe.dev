#' Extract fragments by populations from an ArchR Project
#'
#' \code{getPopFrags} returns a list of sample-specific fragments per cell population as a GRangesList.
#'
#' @param ArchRProj The ArchR Project.
#' @param cellPopLabel The name of the metadata column of the ArchR Project that contains the populations
#'   of cells you want to extract fragments from.
#' @param cellSubsets Default is 'ALL'. If you want to export only some populations,
#'   then give it a list of group names. This needs to be unique - no duplicated
#'   names. This list of group names must be identical to names that appear in
#'   the given cellPopLabel metadata column of the ArchR Project.
#' @param poolSamples Set TRUE to pool sample-specific fragments by cell population. By default this is FALSE and sample-specific fragments are returned.
#' @param numCores Number of cores to use.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#'
#' @return A list of GRanges containing fragments. Each GRanges corresponds to a
#'   population defined by cellSubsets and sample.
#'
#' @export

getPopFrags <- function(ArchRProj,
                        cellPopLabel,
                        cellSubsets = "ALL",
                        poolSamples = FALSE,
                        numCores = 1,
                        verbose = FALSE) {
  nFrags <- NULL
  # Turn off ArchR logging messages
  suppressMessages(ArchR::addArchRVerbose(verbose = FALSE))

  # Extract metadata
  metadf <- ArchR::getCellColData(ArchRProj)

  if (!any(colnames(metadf) %in% cellPopLabel)) {
    stop(paste(
      stringr::str_interp("Provided cellPopLabel ${cellPopLabel} does not exist in the cellColData of your ArchRProj."),
      stringr::str_interp("Available columns are: ${colnames(metadf)}")
    ))
  }

  # Get cell counts for all cell populations
  tmp <- metadf[, cellPopLabel]
  cellCounts <- table(tmp[!is.na(tmp)])

  if (all(tolower(cellSubsets) == "all")) {
    cellPopulations <- names(cellCounts)
  } else {

    # Take our cell populations from given cellSubsets
    cellPopulations <- unique(cellSubsets[!is.na(cellSubsets)][order(cellSubsets)])

    # Filter cellCounts to selected cell subsets (populations)
    cellCounts <- cellCounts[cellPopulations]
    if (any(is.na(cellCounts))) {
      stop(paste(
        "Some cell populations have NA cell counts.",
        "Please verify that all given cellSubsets exist for the given cellPopLabel.",
        stringr::str_interp("cellSubsets with NA cell counts: ${cellPopulations[is.na(names(cellCounts))]}")
      ))
    }
  }

  if (length(cellCounts) == 0) {
    stop("No cells were found for the given cellSubsets and/or cellPopLabel.")
  }

  cellNames <- rownames(metadf)[metadf[, cellPopLabel] %in% cellPopulations]

  #### Up to here, the above is only specific to sampleSpecific=False
  allArrows <- ArchR::getArrowFiles(ArchRProj)
  # Get sample names from our populations of interest
  samplesToExtract <- paste(
    unique(metadf$Sample[which(metadf[, cellPopLabel] %in% cellPopulations)]),
    collapse = "|"
  )
  # Get arrows containing the samples we want
  arrows <- allArrows[grepl(samplesToExtract, allArrows)]

  if (length(arrows) == 0) {
    stop(paste(
      "Found no arrows containing samples from your selected cell subset(s).",
      "Check cellSubsets or the input ArchR Project."
    ))
  }


  # Extract fragments from all available regions
  arrowList <- lapply(seq_along(arrows), function(x) {
    list(arrows[x], grep(names(arrows)[x], cellNames, value = TRUE))
  })
  if (verbose) {
    message("Extracting fragments from arrow files")
  }

  frags <- pbapply::pblapply(arrowList, simplifiedFragments, cl = numCores)

  # From MOCHA - sorts cell barcodes by population
  barcodesByCellPop <- lapply(cellPopulations, function(x) {
    row.names(metadf)[which(metadf[, cellPopLabel] == x)]
  })

  # Clean up cell populations
  # PROTECTED DELIMITER CHARACTERS: # and . and __
  cleanedCellPopulations <- gsub("[#.]|_{2}", "_", cellPopulations)
  changedLabelsBool <- grepl("[#.]|_{2}", cellPopulations)
  if (any(changedLabelsBool)) {
    warning(
      "The following cell population labels contain protected delimiter ",
      "characters '#.' and have been updated. Please use the updated ",
      "cell population labels in downstream analyses. ",
      "\nPrevious: \n", paste0(cellPopulations[changedLabelsBool], collapse = "  "),
      "\nUpdated: \n", paste0(cleanedCellPopulations[changedLabelsBool], collapse = "  ")
    )
  }
  names(barcodesByCellPop) <- cleanedCellPopulations
  cellPopulations <- cleanedCellPopulations

  # If you are extracting more than one population, then sort the fragments by population.
  # If you are not, then no sorting is necessary.
  if (length(cellPopulations) > 1 | all(tolower(cellPopulations) == "all")) {
    # Set up sets for sorting by fragments for cell types
    fragIterList <- lapply(seq_along(frags), function(x) {
      list(barcodesByCellPop, frags[[x]])
    })
    rm(frags)
    names(fragIterList) <- names(arrows)

    if (verbose) {
      message("Sorting fragments by cell type.")
    }

    # Subset fragments by cell type
    tmp_fragList <- pbapply::pblapply(fragIterList, subset_Frag, cl = numCores)
    names(tmp_fragList) <- names(arrows)

    if (poolSamples) {
      pooledFrags <- lapply(cellPopulations, function(cellPop) {
        cellPopFrags <- lapply(tmp_fragList, function(x) {
          x[[cellPop]]
        })
        IRanges::stack(methods::as(cellPopFrags, "GRangesList"))
      })
      names(pooledFrags) <- cellPopulations
      return(GenomicRanges::GRangesList(pooledFrags))
    }

    tmp_fragList <- unlist(tmp_fragList, recursive = FALSE)
    # Here we use . and # as protected delimiter characters
    names(tmp_fragList) <- gsub("\\.", "#", names(tmp_fragList))
    sampleName <- gsub("#.*", "", names(tmp_fragList))
    cellTypeName <- gsub(".*#", "", names(tmp_fragList))
    names(tmp_fragList) <- paste(cellTypeName, "#", sampleName, sep = "")
    rm(fragIterList)
  } else {
    # One cell population
    tmp_fragList <- frags

    if (poolSamples) {
      pooledFrags <- list(IRanges::stack(methods::as(tmp_fragList, "GRangesList")))
      names(pooledFrags) <- cellPopulations
      return(GenomicRanges::GRangesList(pooledFrags))
    }

    names(tmp_fragList) <- paste0(cellPopulations, "#", names(arrows))
    rm(frags)
  }

  # Add normalization factor.
  # Double underscore __ is protected delimeter
  fragLength <- unlist(lapply(tmp_fragList, length))
  names(tmp_fragList) <- paste(
    names(tmp_fragList),
    "__",
    fragLength / 10^6,
    sep = ""
  )

  if (length(cellPopulations) > 1 | all(tolower(cellPopulations) == "all")) {
    popFrags <- unlist(lapply(names(barcodesByCellPop), function(x) {
      subsetFrags <- tmp_fragList[grepl(x, names(tmp_fragList))]
      names(subsetFrags) <- grep(x, names(tmp_fragList), value = TRUE)
      subsetFrags
    }))
  } else {
    popFrags <- tmp_fragList
  }

  rm(tmp_fragList)

  return(GenomicRanges::GRangesList(popFrags))
}

#' Extract fragments from an arrow file based on one variable
#'
#' \code{simplifiedFragments} returns a list of fragments for a given set of cell names
#'
#' @param ref a list where the first index is the name of the arrow and the second index is a vector of strings describing cell names
#'
#' @return A GRanges object for all fragments from a set of cells within a given arrow file.
#'
#'
#' @noRd

simplifiedFragments <- function(ref) {
  arrows <- ref[[1]]
  cellNames <- ref[[2]]

  if (length(ref) > 2) {
    regionGRanges <- ref[[4]]
    chrom <- ref[[3]]
    frags <- ArchR::getFragmentsFromArrow(
      ArrowFile = arrows,
      cellNames = cellNames,
      chr = chrom,
      verbose = FALSE
    )
    frags <- plyranges::filter_by_overlaps(frags, regionGRanges)
  } else {
    frags <- ArchR::getFragmentsFromArrow(
      ArrowFile = arrows,
      cellNames = cellNames,
      verbose = FALSE
    )
  }
  return(frags)
}

#' subsets fragments out by cellnames.
#'
#' \code{subset_Frag} returns a sorted set of fragments by populations based on cell barcode lists
#'
#' @param ref a list where the first index is the cellnames for that population, and the second index is a GRanges of fragments
#'
#' @return A Granges object for all fragments from a set of cells within a given arrow file.
#'
#'
#' @noRd

# From MOCHA - Function to sort fragments by populations based on cell barcode lists
subset_Frag <- function(ref) {
  barcodesByCellPop <- ref[[1]]
  fragsGRanges <- ref[[2]]

  fragsTable <- data.table::as.data.table(fragsGRanges)
  sortedFrags <- lapply(barcodesByCellPop, function(y) {
    idx <- which(fragsTable$RG %in% y)
    fragsGRanges[idx]
  })

  names(sortedFrags) <- names(barcodesByCellPop)
  return(sortedFrags)
}
