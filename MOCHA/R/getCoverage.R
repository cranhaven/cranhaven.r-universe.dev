#' @title Get sample-specific coverage files for each sample-cell population.
#'
#' @description getCoverage takes the output of MOCHA::getPopFrags and returns
#'  a GRanges of singe-basepair resolution coverage.
#'
#' @param popFrags GRangesList of fragments for all sample/cell populations
#' @param normFactor Normalization factor. Can be either be one, in which case all coverage files will be normalized by the same value, or the same length as the GRangesList
#' @param TxDb The TxDb-class transcript annotation
#'   package for your organism (e.g. "TxDb.Hsapiens.UCSC.hg38.refGene"). This
#'   must be installed. See
#'   \href{https://bioconductor.org/packages/release/data/annotation/}{
#'   Bioconductor AnnotationData Packages}.
#' @param cl cl argument to \code{\link[pbapply]{pblapply}}
#' @param filterEmpty True/False flag on whether or not to carry forward regions without coverage.
#' @param verbose Boolean variable to determine verbosity of output.
#'
#' @return popCounts A GRangesList of coverage for each sample and cell population
#' @export
getCoverage <- function(popFrags, normFactor, TxDb, cl, filterEmpty = FALSE, verbose = FALSE) {
  score <- NULL
  if (length(normFactor) == 1) {
    normFactor <- rep(normFactor, length(popFrags))
  } else if (length(normFactor) != length(popFrags)) {
    stop("Length of normFactor is equal to length of popFrags. Please either give 1 value, or a vector of equal length to popFrags.")
  }

  if (verbose) {
    message(paste("Counting", paste0(names(popFrags), collapse = ", "), sep = " "))
  }

  popFragList <- lapply(seq_along(popFrags), function(x) {
    list(popFrags[[x]], normFactor[[x]], filterEmpty)
  })


  # Summarize the coverage over the region window at a single basepair resolution
  popCounts <- pbapply::pblapply(popFragList, calculateCoverage, cl = cl)
  # Summarize the coverage over the region window at a single basepair resolution
  insertCounts <- pbapply::pblapply(popFragList, calculateInsertionCoverage, cl = cl)

  popCounts <- lapply(popCounts, function(x) {
    GenomeInfoDb::seqinfo(x) <- GenomeInfoDb::seqinfo(TxDb)[GenomicRanges::seqnames(GenomeInfoDb::seqinfo(x))]
    x
  })
  names(popCounts) <- names(popFrags)

  insertCounts <- lapply(insertCounts, function(x) {
    GenomeInfoDb::seqinfo(x) <- GenomeInfoDb::seqinfo(TxDb)[GenomicRanges::seqnames(GenomeInfoDb::seqinfo(x))]
    x
  })
  names(insertCounts) <- names(popFrags)

  return(list("Accessibility" = popCounts, "Insertions" = insertCounts))
}

#' @title Take in a GRanges object and generates coverage GRanges.
#' @param ref GRanges object
#' @noRd
calculateCoverage <- function(ref) {
  score <- NULL
  popFrags <- ref[[1]]
  Num <- ref[[2]]
  filterEmpty <- ref[[3]]

  counts_gr <- plyranges::compute_coverage(popFrags) %>% plyranges::mutate(score = score / Num)

  if (filterEmpty) {
    plyranges::filter(counts_gr, score > 0)
  } else {
    counts_gr
  }
}

#' @title Compute the average coverage intensity for specific regions.
#' @param covFiles GRangesList of coverage for each sample
#' @param regions Regions to count intensities over, must be non-overlapping and non-adjacent ( > 1 bp apart).
#' @param numCores number of cores to parallelize over.
#'
#' @noRd
getSpecificCoverage <- function(covFiles, regions, numCores = 1) {
  score <- NewScore <- WeightedScore <- . <- NULL
  counts <- parallel::mclapply(covFiles, function(x) {
    x %>%
      plyranges::mutate(NewScore = score) %>%
      plyranges::join_overlap_intersect(regions) %>%
      plyranges::mutate(WeightedScore = NewScore * GenomicRanges::width(.)) %>%
      plyranges::reduce_ranges(score = mean(WeightedScore))
  }, mc.cores = numCores)

  return(counts)
}


# helper function that takes in a GRanges fragment object and generates coverage GRanges for insertions.
calculateInsertionCoverage <- function(ref) {
  score <- NULL
  popFrags <- ref[[1]]
  Num <- ref[[2]]
  filterEmpty <- ref[[3]]

  cutstart <- GenomicRanges::GRanges(
    seqnames = methods::as(GenomicRanges::seqnames(popFrags), "vector"),
    ranges = IRanges::IRanges(start = IRanges::start(popFrags), width = 1), strand = "*"
  )
  cutend <- GenomicRanges::GRanges(
    seqnames = methods::as(GenomicRanges::seqnames(popFrags), "vector"),
    ranges = IRanges::IRanges(start = IRanges::end(popFrags), width = 1), strand = "*"
  )

  counts_gr <- plyranges::compute_coverage(plyranges::bind_ranges(cutstart, cutend))
  counts_gr <- plyranges::mutate(counts_gr, score = score / Num)

  if (filterEmpty) {
    plyranges::filter(counts_gr, score > 0)
  } else {
    counts_gr
  }
}
