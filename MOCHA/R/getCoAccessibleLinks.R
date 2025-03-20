#' @title \code{getCoAccessibleLinks}
#'
#' @description \code{getCoAccessibleLinks} takes an input set of regions (tiles) and finds co-accessible neighboring regions within a window. Co-accessibility is defined as the correlation between two region intensity (openness) across samples.
#'
#'
#' @param SampleTileObj The SummarizedExperiment object output from getSampleTileMatrix containing your sample-tile matrices
#' @param cellPopulation A string denoting the cell population of interest, which must be present in SampleTileObj
#' @param regions a GRanges object or vector or strings containing the regions on which to compute co-accessible links. Strings must be in the format "chr:start-end", e.g. "chr4:1300-2222".
#'   Can be the output from getDifferentialAccessibleTiles.
#' @param chrChunks This functions subsets by groups of chromosome, and then parallelizes within each group of chromosomes when running correlations. This method keeps memory
#'   low. To speed things up on high performing platforms, you can chunk out more than one chromosome at a time. Default is chrChunks = 1, so only one chromosome at a time.
#' @param windowSize the size of the window, in basepairs, around each input region to search for co-accessible links
#' @param numCores Optional, the number of cores to use with multiprocessing. Default is 1.
#' @param verbose Set TRUE to display additional messages. Default is FALSE.
#' @param approximateTile If set to TRUE, it will use all tiles that overlap with the regions given, instead of finding an exact match to the regions variable. Default is FALSE.
#' @param ZI boolean flag that enables zero-inflated (ZI) Spearman correlations to be used. Default is TRUE. If FALSE, skip zero-inflation and calculate the normal Spearman.
#'
#' @return TileCorr A data.table correlation matrix
#'
#' @details The technical details of the zero-inflated correlation can be
#'          found here:
#'
#'               Pimentel, Ronald Silva, "Kendall's Tau and Spearman's Rho
#'               for Zero-Inflated Data" (2009). Dissertations.
#'
#'          while the implementation (scHOT R package), can be found here:
#'               http://www.bioconductor.org/packages/release/bioc/html/scHOT.html
#'
#'
#' @export
getCoAccessibleLinks <- function(SampleTileObj,
                                 cellPopulation = "All",
                                 regions,
                                 chrChunks = 1,
                                 windowSize = 1 * 10^6,
                                 numCores = 1,
                                 ZI = TRUE,
                                 approximateTile = FALSE,
                                 verbose = FALSE) {
  . <- NULL

  # verify input
  if (methods::is(regions, "GenomicRanges")) {
    regionDF <- as.data.frame(regions)
  } else if (methods::is(regions, "character")) {
    regionDF <- MOCHA::StringsToGRanges(regions) %>% as.data.frame()
    regions <- MOCHA::StringsToGRanges(regions)
  } else {
    stop('Invalid input type for "region": must be either "GRanges" or a character vector')
  }

  if (approximateTile) {
    regions <- plyranges::filter_by_overlaps(SummarizedExperiment::rowRanges(SampleTileObj), regions)
    regionDF <- as.data.frame(regions)
  } else if (
    (!(all(IRanges::width(regions) == 500) &&
      all((IRanges::end(regions) + 1) %% 500 == 0)))
  ) {
    # MOCHA tiles always end at 1 less than a
    # number divisible by 500 e.g. 499.
    # Using end positions in the case that 0-499 is a tile.
    stop("The candidate regions don't match exact tiles from MOCHA. To find the closest overlapping tiles, set approximate to TRUE")
  }

  if (!methods::is(chrChunks, "numeric")) {
    stop("chrChunks is not numeric")
  } else if (chrChunks %% 1 != 0 || chrChunks <= 0) {
    stop("chrChunks is not a positive integer value. Please set chrChunks to be a positive integer.")
  }

  if (cellPopulation == "All") {
    tileDF <- do.call("cbind", as.list(SummarizedExperiment::assays(SampleTileObj)))
  } else if (length(cellPopulation) > 1 && all(cellPopulation %in% names(SummarizedExperiment::assays(SampleTileObj)))) {
    tileDF <- do.call("cbind", SummarizedExperiment::assays(SampleTileObj)[names(SummarizedExperiment::assays(SampleTileObj)) %in% cellPopulation]) #
  } else if (length(cellPopulation) == 1 && all(cellPopulation %in% names(SummarizedExperiment::assays(SampleTileObj)))) {
    tileDF <- MOCHA::getCellPopMatrix(SampleTileObj, cellPopulation, NAtoZero = TRUE) #
  } else {
    stop("Cell type not found within SampleTileObj")
  }

  tileNames <- rownames(tileDF)

  regionsStrings <- GRangesToString(regions)
  if (!all(regionsStrings %in% tileNames)) {
    stop(
      "Invalid regions provided. All given regions must be tiles in your SampleTileObj. ",
      "\nThe following regions are not tiles in the SampleTileObj:\n",
      paste(regionsStrings[which(!regionsStrings %in% tileNames)], collapse = ", ")
    )
  }
  start <- as.numeric(gsub("chr.*\\:|\\-.*", "", tileNames))
  end <- as.numeric(gsub("chr.*\\:|.*\\-", "", tileNames))
  chr <- gsub("\\:.*", "", tileNames)


  # Find all combinations to test
  if (verbose) {
    message("Finding all tile pairs to correlate.")
  }

  cl <- parallel::makeCluster(numCores)

  iterList <- lapply(seq_len(dim(regionDF)[1]), function(x) {
    list(regionDF[1, ], start, end, chr, windowSize)
  })

  allCombinations <- pbapply::pblapply(cl = cl, X = iterList, FUN = findAllCombinations) %>%
    do.call("rbind", .) %>%
    dplyr::distinct()



  # Determine chromosomes to search over, and the number of iterations to run through.
  chrNum <- paste(unique(regionDF$seqnames), ":", sep = "")
  numChunks <- length(chrNum) %/% chrChunks
  numChunks <- ifelse(length(chrNum) %% chrChunks == 0, numChunks, numChunks + 1)

  if (verbose) {
    message("Finding subsets of pairs for testing.")
  }

  iterList <- lapply(1:numChunks, function(y) {
    list(y, chrNum, chrChunks, tileNames, allCombinations$Key)
  })
  # Find all indices for subsetting (indices of allCombinations and indices of the tileDF)
  combList <- pbapply::pblapply(cl = cl, X = iterList, FUN = splitCombList)

  # Initialize zi_spear_mat for iterations
  zi_spear_mat <- NULL

  for (i in 1:numChunks) {
    subTileDF <- tileDF[combList[[i]][[1]], , drop = FALSE]
    subCombinations <- allCombinations[combList[[i]][[2]], ]


    if (!all(subCombinations[, "Key"] %in% rownames(subTileDF))) {
      return(list(subCombinations, subTileDF))
    }

    if (verbose) {
      message(paste("Finding correlations for Chromosome(s)", gsub("chr", "", combList[[i]][[3]]), sep = " "))
    }

    if (!all(subCombinations[, 1] %in% rownames(subTileDF) | subCombinations[, 2] %in% rownames(subTileDF))) {
      stop("subset of pair combinations and tile data.frame does not match.")
    }

    zi_spear_mat_tmp <- runCoAccessibility(subTileDF, subCombinations, ZI, verbose, cl)

    gc()

    zi_spear_mat <- rbind(zi_spear_mat, zi_spear_mat_tmp)
  }

  parallel::stopCluster(cl)

  return(zi_spear_mat)
}


findAllCombinations <- function(iterList) {

  # Extract info needed
  reg <- iterList[[1]]
  start <- iterList[[2]]
  end <- iterList[[3]]
  chr <- iterList[[4]]
  windowSize <- iterList[[5]]
  tileNames <- paste0(chr, ":", start, "-", end, paste = "")

  keyTile <- which(start == reg$start &
    end == reg$end &
    chr == reg$seqnames)

  windowIndexBool <- which(start > reg$start - windowSize / 2 &
    end < reg$end + windowSize / 2 &
    chr == reg$seqnames)

  windowIndexBool <- windowIndexBool[windowIndexBool != keyTile]

  if (length(windowIndexBool) > 0) {
    # Var1 will always be our region of interest
    keyNeighborPairs <- data.frame(
      "Key" = tileNames[keyTile],
      "Neighbor" = tileNames[windowIndexBool]
    )
  } else {
    keyNeighborPairs <- NULL
  }

  return(keyNeighborPairs)
}

splitCombList <- function(iterList) {
  # input is list(y, chrNum, chrChunks, tileNames, allCombinations$key)
  index <- iterList[[1]]
  chrNum <- iterList[[2]]
  chrChunks <- iterList[[3]]
  tileNames <- iterList[[4]]
  key <- iterList[[5]]

  specChr <- paste0(chrNum[which(c(seq_along(chrNum)) > (index - 1) * chrChunks &
    c(seq_along(chrNum) <= index * chrChunks))], collapse = "|")

  tileIndices <- grep(specChr, tileNames)
  combIndices <- grep(specChr, key)

  infoList <- list(tileIndices, combIndices, specChr)

  return(infoList)
}
