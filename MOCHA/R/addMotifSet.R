#' @title \code{addMotifSet}
#'
#' @description \code{addMotifSet} Identify motifs within your peakset.
#'
#' @param SampleTileObj A SummarizedExperiment, specifically the output of
#'   getSampleTileMatrix
#' @param motifPWMs A pwms object for the motif database. Either PFMatrix,
#'   PFMatrixList, PWMatrix, or PWMatrixList
#' @param w Parameter for motifmatchr controlling size in basepairs of window for filtration.
#'   Default is 7.
#' @param returnSTM If TRUE, return the modified SampleTileObj with motif set
#'   added to metadata (default). If FALSE, return the motifs from motifmatchr as a GRanges.
#' @param motifSetName Name to give motifList in the SampleTileObj's metadata
#'   if `returnSTM=TRUE`. Default is 'Motifs'.
#'
#' @return the modified SampleTileObj with motifs added to the metadata
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # load a curated motif set from library(chromVARmotifs)
#' # included with ArchR installation
#' data(human_pwms_v2)
#' SE_with_motifs <- addMotifSet(
#'   SampleTileObj,
#'   motifPWMs = human_pwms_v2,
#'   returnSTM = TRUE, motifSetName = "Motifs", w = 7
#' )
#' }
#'
#' @export
#'
addMotifSet <- function(SampleTileObj,
                        motifPWMs,
                        w = 7,
                        returnSTM = TRUE,
                        motifSetName = "Motifs") {
  if (!requireNamespace("motifmatchr", quietly = TRUE)) {
    stop(
      "Package 'motifmatchr' is required for addMotifSet. ",
      "Please install 'motifmatchr' to proceed."
    )
  }
  TotalPeakSet <- SummarizedExperiment::rowRanges(SampleTileObj)

  genome <- S4Vectors::metadata(SampleTileObj)$Genome
  genome <- BSgenome::getBSgenome(genome)

  motif_ix <- motifmatchr::matchMotifs(
    pwms = motifPWMs,
    subject = TotalPeakSet,
    genome = genome,
    out = "positions", w = w
  )

  . <- NULL
  names(motif_ix) <- sub("_D_.*|_I_.*", "", names(motif_ix)) %>%
    sub("_I$|_D$", "", .) %>%
    sub(".*_LINE", "", .) %>%
    sub(".*_", "", .)

  motifList <- list(motif_ix)
  names(motifList) <- motifSetName

  if (returnSTM) {
    S4Vectors::metadata(SampleTileObj) <- append(
      S4Vectors::metadata(SampleTileObj), motifList
    )
    return(SampleTileObj)
  } else {
    return(motif_ix)
  }
}
