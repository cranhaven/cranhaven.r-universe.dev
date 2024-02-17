#' Get conservation scores for variants
#' @keywords internal
#' @param variantsTable A variant table, positions are 1-based
#' @param conservationBwPath The path of the bigWig conservation track file download from UCSC Genome Browser
#' @return A vector of strings, which are conservation scores of input variants. If a variant reference sequence has more than 1 nucleotide, the conservation scores of all its positions will be concatenated with ";".
#' @importFrom rtracklayer import
#' @importFrom GenomicRanges GRanges
#' @importFrom dplyr transmute left_join
get_conservation_scores <- function(variantsTable, conservationBwPath) {
  # function to return a data frame of coords and their phastCons or phyloP scores
  # Args:
  #   variantsTable: variantsTable: variants table, positions are 1-based
  #   conservationBwPath: the path of the bigWig conservation track file download from UCSC Genome Browser
  # Return:
  #   a string concatenated their phastCons or phyloP scores with ";"

  # start and end are 1-based full closed (start and end both included)
  Chr <- Pos <- Ref <- NULL
  coords <- variantsTable %>%
    transmute(chrom = Chr, start = Pos, end = as.numeric(start+nchar(Ref))-1)
  # if chrom doesn't start with chr, add chr prefix
  if (all(str_starts(coords[[1]], "chr")) == FALSE) {
    coords[!str_starts(coords[[1]], "chr"),1] <- str_c("chr", coords[[1]][!str_starts(coords[[1]], "chr")])
  }
  scores_out <- c()
  for (i in 1:nrow(coords)) {
    scores_df <- data.frame(pos = seq(coords[[i,2]], coords[[i,3]]))
    scores_vec <- rtracklayer::import.bw(con = conservationBwPath, which = GRanges(paste0(coords[[i, 1]], ":", coords[[i,2]], "-", coords[[i,3]])))
    if (length(scores_vec) == 0) {
      scores_out <- append(scores_out, paste(rep("NA", nrow(scores_df)), collapse = ";"))
      next
    }
    final_score <- data.frame(pos = start(scores_vec), conservation = round(scores_vec$score, 3))
    scores_df <- left_join(scores_df, final_score, by=c("pos"="pos"))
    scores_out <- append(scores_out, paste(scores_df$conservation, collapse = ";"))
  }
  return (scores_out)
}
