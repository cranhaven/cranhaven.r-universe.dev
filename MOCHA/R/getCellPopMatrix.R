#' @title \code{getCellPopMatrix}
#'
#' @description \code{getCellPopMatrix} pulls out the SampleTileMatrix of tiles
#'   called in one given cell population.
#'
#' @param SampleTileObj The output from getSampleTileMatrix, a
#'   SummarizedExperiment of pseudobulk intensities across all tiles & cell
#'   types.
#' @param cellPopulation The cell population you want to pull out.
#' @param dropSamples Boolean flag to determine whether to drop samples that
#'   were too small for peak calling.
#' @param NAtoZero Boolean flag to determine whether to replace NAs with zero
#'
#' @return sampleTileMatrix a matrix of samples by called tiles for a given cell
#'   population.
#'
#' @export
getCellPopMatrix <- function(SampleTileObj,
                             cellPopulation,
                             dropSamples = TRUE,
                             NAtoZero = TRUE) {
  tilesCalled <- GenomicRanges::mcols(
    SummarizedExperiment::rowRanges(SampleTileObj)
  )[, cellPopulation]

  sampleTileMatrix <- SummarizedExperiment::assays(
    SampleTileObj
  )[[cellPopulation]][tilesCalled, , drop = FALSE]

  # Drop empty samples

  if (dropSamples) {
    sampleTileMatrix <- sampleTileMatrix[
      , colSums(sampleTileMatrix, na.rm = TRUE) > 0,
      drop = FALSE
    ]
  }

  if (NAtoZero) {
    sampleTileMatrix[is.na(sampleTileMatrix)] <- 0
  }

  return(sampleTileMatrix)
}
