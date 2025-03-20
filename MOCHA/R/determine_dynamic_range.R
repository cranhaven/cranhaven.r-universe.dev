#' @title \code{determine_dynamic_range}
#'
#' @description \code{determine_dynamic_range} is an R helper function, part of the single-cell peak calling
#' algorithm MOCHA by (Zaim, Pebworth, et. al. 2022) that determines which genomic regions, or bins,
#' will be used for de-novo peak calling. The function "determine_dynamic_range" is a wrapper function
#' around the dynamic_bins function
#'
#'
#' @param AllFragmentsList: List of fragments by arrow file
#' @param blackList A GRanges object containing a blacklist of regions to exclude
#' @param GeneralWindowSize: Window size for sliding window generated over longer fragments.
#' @param WindowSizeRange: The sliding window function will generate a smaller window at the end of a longer fragment,
# if the fragment is not evenly divisible by GeneralWindowSize. If that smaller window is less than or equal to
# WindowSizeRange, then it will be merged with the preceding window to great a larger window. This means that
# longer fragments will be broken up into bins that are between WindowSizeRange and WindowSizeRange+GeneralWindowSize in length.
#' @param doBins is a boolean variable. When true, then it will into windows according to GeneralWindowSize and WindowSizeRange.
#' @return a data.table, countsByBin, that returns the two intensity parameters required
#' to calculate the probability of a (+) peak
#'
#' @details The technical details of the algorithm are found in XX.
#'
#' @noRd
#'


determine_dynamic_range <- function(AllFragmentsList, blackList, binSize = 500, doBin = FALSE) {
  . <- NULL
  # if(class(AllFragmentsList)!='SimpleList'){
  #   stop('AllFragmentsList must be a list of arrow files')
  # }

  TotalRange <- dynamic_bins(
    AllFragmentsList = AllFragmentsList,
    doBin = doBin
  )

  if (!methods::is(binSize, "numeric") | binSize < 0) {
    stop(paste('invalid binSize!: binSize must be an integer value > 0 indicating the width of the genomic region check "binSize=',
      binSize, '" input',
      sep = ""
    ))
  }

  if (!methods::is(doBin, "logical")) {
    stop("doBin user-input must be a TRUE/FALSE boolean input")
  }

  # Let's subtract out areas of fragments that overlap with blacklist regions.
  # Let's not remove the region entirely, because the regions may be long or only

  TotalRangesFilt <- plyranges::setdiff_ranges(TotalRange, blackList)

  RangeBins <- plyranges::stretch(plyranges::anchor_end(TotalRangesFilt),
    extend = GenomicRanges::start(TotalRangesFilt) %% binSize
  )

  FinalBins <- plyranges::stretch(plyranges::anchor_start(RangeBins),
    extend = (binSize - IRanges::end(RangeBins) %% binSize)
  ) %>%
    plyranges::reduce_ranges() %>%
    plyranges::slide_ranges(width = binSize, step = binSize) %>%
    plyranges::filter(IRanges::width(.) == binSize)

  FinalBins <- IRanges::subsetByOverlaps(FinalBins, blackList, invert = T)

  return(FinalBins)
}
