#' @title \code{calculate_intensities}
#'
#' @description \code{calculate_intensities} is an R helper function, part of the single-cell peak calling
#' algorithm MOCHA by (Zaim, Pebworth, et. al. 2022) that calculates the design matrix, X,
#' used to make peak calls.
#'
#' @param fragMat a genomic ranges object containing the fragments
#' @param candidatePeaks a genomic ranges object indicating where to call peaks. This could be a pre-selected peak set or a dynamic bins approach to searching the whole genome.
#' @param totalFrags a numeric normalizing value, indicating the total # of fragments for a cell pop'n in a sample
#'
#' @return a data.table, countsByBin, that returns the two intensity parameters required
#' to calculate the probability of a (+) peak
#'
#' @details The technical details of the algorithm are found in XX.
#'
#' @usage calculate_intensities(fragMat, candidatePeaks, FALSE)
#'
#' @import data.table
#'
#' @noRd
#'

calculate_intensities <- function(fragMat,
                                  candidatePeaks,
                                  totalFrags,
                                  cellCol = "RG",
                                  verbose = FALSE) {
  if (!methods::is(fragMat, "GRanges")) {
    stop("Input fragMat must be a Genomic Ranges (GRanges) object")
  }

  if (!methods::is(candidatePeaks, "GRanges")) {
    stop("Input candidatePeaks must be a Genomic Ranges (GRanges) object")
  }
  if (!is.integer(totalFrags)) {
    stop("totalFrags user-input must be an integer denoting the total # of frags")
  }
  bin <- cell <- normedFrags <- NULL
  ### set normalization
  ### scale for fragment counts
  normScale <- 10^9

  ## transform fragments into data.table
  fragMat_dt <- data.table::as.data.table(fragMat)

  ## transform granges to data.table
  candidatePeaksDF <- data.table::as.data.table(candidatePeaks)
  candidatePeaksDF$bin <- paste(candidatePeaksDF$seqnames, ":", candidatePeaksDF$start,
    "-", candidatePeaksDF$end,
    sep = ""
  )

  ### identify overlaps between
  ### fragments & candidate peak regions
  fragsPerBin <- GenomicRanges::findOverlaps(fragMat,
    candidatePeaks,
    minoverlap = 0
  )

  ### Convert overlap matrix into DT
  fragsPerBin <- data.table::as.data.table(fragsPerBin)

  ### Get Cell Counts
  numCells <- length(unique(fragMat_dt[[cellCol]]))

  ### Label Cell Name
  fragsPerBin$cell <- fragMat_dt[[cellCol]][fragsPerBin$queryHits]

  ### Get Window, Chr, Start, End and Strand information
  ### From the Overlaps matrix
  fragsPerBin$bin <- candidatePeaksDF$bin[fragsPerBin$subjectHits]
  fragsPerBin$chr <- candidatePeaksDF$seqnames[fragsPerBin$subjectHits]
  fragsPerBin$strand <- candidatePeaksDF$strand[fragsPerBin$subjectHits]

  fragsPerBin$start <- candidatePeaksDF$start[fragsPerBin$subjectHits]
  fragsPerBin$end <- candidatePeaksDF$end[fragsPerBin$subjectHits]
  fragsPerBin$width <- candidatePeaksDF$width[fragsPerBin$subjectHits]

  ### Convert frags per bin matrix into
  ### a data.table for obtaining cell counts
  fragsPerBin <- data.table::as.data.table(fragsPerBin)

  ### get cell count matrix
  cell_counts <- fragsPerBin[, .N, by = list(bin, cell)]

  ### include normalized counts
  cell_counts$normedFrags <- cell_counts$N / (totalFrags / normScale)

  #### doing bin-level summaries
  setkey(cell_counts, bin)

  #### calculate pseudoBulk intensity features
  countsByBin <- cell_counts[, list(
    TotalIntensity = sum(normedFrags),
    maxIntensity = max(normedFrags)
  ), by = bin]

  ### join to the original dynamic bins
  ### to retain original variables

  countsByBin <- dplyr::left_join(candidatePeaksDF[, c("strand", "seqnames", "start", "end", "bin")],
    countsByBin,
    by = "bin"
  )

  ### if the dynamic bins is calculated
  ### on a cohort, and applied to samples
  ### some entries will be NAs
  ### and this will set NAs to 0 counts

  countsByBin[is.na(countsByBin)] <- 0

  ### order by chromosome
  countsByBin <- countsByBin[order(countsByBin$seqnames, countsByBin$start, decreasing = FALSE), ]
  countsByBin$numCells <- numCells

  chromosomeOrder <- c(
    "chr1", "chr2", "chr3", "chr4",
    "chr5", "chr6", "chr7", "chr8",
    "chr9", "chr10", "chr11", "chr12",
    "chr13", "chr14", "chr15", "chr16",
    "chr17", "chr18", "chr19", "chr20",
    "chr21", "chr22", "chrX", "chrY"
  )

  ### Order Chromosome Names
  countsByBin$seqnames <- factor(countsByBin$seqnames, levels = chromosomeOrder, ordered = T)
  countsByBin <- countsByBin[order(countsByBin$seqnames, countsByBin$start, decreasing = FALSE), ]

  ### retain only features of interest and in order required
  countsByBin <- countsByBin[, c(
    "bin", "seqnames", "start", "end", "strand",
    "TotalIntensity", "maxIntensity", "numCells"
  ), with = F]

  ### Rename "bin" identifier as "tileID"
  colnames(countsByBin)[1] <- "tileID"
  if (verbose) {
    message(" ---> Analysis finished on ", numCells, " cells")
  }

  return(countsByBin)
}
