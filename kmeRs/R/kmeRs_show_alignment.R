#' @title Calculate and Show Alignment Between Two Compared K-mers
#'
#' @description
#' The \code{kmeRs_show_alignment} function aligns and shows calculated
#' alignment between two DNA or RNA sequences
#'
#' @aliases kmeRs_show_alignment
#'
#' @param kmer_A given k-mer A
#' @param kmer_B given k-mer B
#' @param seq.type type of sequence in question, either 'DNA' or 'AA' (default)
#' @param submat substitution matrix version, defaults to 'BLOSUM62'; other
#' choices include 'BLOSUM45', 'BLOSUM50', 'BLOSUM62', 'BLOSUM80', 'BLOSUM100',
#' 'PAM30', 'PAM40', 'PAM70', 'PAM120' and 'PAM250'; this parameter is ignored
#' if \code{na.match} and \code{na.mismatch} are specified
#' @param align.type "global" or "local" 
#' @param verbose = TRUE 
#' @param na.match for DNA sequences, what should the score for exact match be?
#' @param na.mismatch for DNA sequences, what should the score for mismatches be?
#' @param ... other parameters, e.g. gap opening/extension penalties (\code{gapOpening},
#' \code{gapExtension}) for generating a DNA base substitution matrix 
#'
#' @return alignment is returned as a data frame
#'
#' @examples
#' # Example DNA alignment with gap opening and extension penalties of 1 and 0
#' # with default base match/mismatch values
#'
#' kmeRs_show_alignment(kmer_A = "AAATTTCCCGGG", kmer_B = "TCACCC",
#'     seq.type = "DNA", gapOpening = 1, gapExtension = 0)
#'     
#' @export
kmeRs_show_alignment <- function(kmer_A, kmer_B, seq.type = "AA",
	submat = ifelse(
		test = (match.arg(toupper(seq.type), c("DNA", "AA")) == "AA"),
		yes = "BLOSUM62", no = NA),
	na.match = ifelse(is.na(submat), yes = 2, no = NA),
	na.mismatch = ifelse(is.na(submat), yes = -3, no = NA),
	align.type = "global", verbose = TRUE, ...) {
	
	# this version uses blastn defaults (i.e. PAM90) for na.match and na.mismatch	
	if (missing(kmer_A) | missing(kmer_B)) {
		stop("input k-mers are not specified!")
	}
	
	alignment <- kmeRs_twoSeqSim(kmer_A, kmer_B, seq.type = seq.type,
		na.match = na.match, na.mismatch = na.mismatch, align.type = align.type, verbose = verbose, ...)
	
    # Make it fancy for markdown
    alignment_dataframe <- data.frame(matrix(nrow = 2, ncol = 2), 
		row.names = c("Sequence A", "Sequence B"))
    colnames(alignment_dataframe) <- c("Alignment", "Score")

    alignment_dataframe[1,1] <- as.character(alignment@pattern)
    alignment_dataframe[2,1] <- as.character(alignment@subject)
    alignment_dataframe[c(1,2),2] <- "" #as.character(alignment@score)
    return(alignment_dataframe)
}

#' @title kmeRs_twoSeqSim
#' @description Supporting func to kmeRs_show_alignment
#' 
#' @param kmer_A given k-mer A
#' @param kmer_B given k-mer B
#' @param seq.type type of sequence in question, either 'DNA' or 'AA' (default)
#' @param submat substitution matrix version, defaults to 'BLOSUM62'; other
#' choices include 'BLOSUM45', 'BLOSUM50', 'BLOSUM62', 'BLOSUM80', 'BLOSUM100',
#' 'PAM30', 'PAM40', 'PAM70', 'PAM120' and 'PAM250'; this parameter is ignored
#' if \code{na.match} and \code{na.mismatch} are specified
#' @param align.type "global" or "local" 
#' @param verbose = TRUE 
#' @param na.match for DNA sequences, what should the score for exact match be?
#' @param na.mismatch for DNA sequences, what should the score for mismatches be?
#' @param ... other parameters, e.g. gap opening/extension penalties (\code{gapOpening},
#' \code{gapExtension}) for generating a DNA base substitution matrix 

kmeRs_twoSeqSim <- function(kmer_A, kmer_B, seq.type = "AA",
                            submat = ifelse(
                              test = (match.arg(toupper(seq.type), c("DNA", "AA")) == "AA"),
                              yes = "BLOSUM62", no = NA),
                            na.match = ifelse(is.na(submat), yes = 2, no = NA),
                            na.mismatch = ifelse(is.na(submat), yes = -3, no = NA), align.type = "global", verbose = TRUE, ...) {	
  # recreation of rDNAse with added checks for nucleic acids
  if (seq.type == "DNA") {
    if (!is.na(na.match) & !is.na(na.mismatch)) {
      kmer_A <- Biostrings::DNAString(kmer_A)
      kmer_B <- Biostrings::DNAString(kmer_B)
      align.mat <- pwalign::nucleotideSubstitutionMatrix(
        match = na.match, mismatch = na.mismatch, baseOnly = TRUE)
      #return(align.mat)
      alignment <- pwalign::pairwiseAlignment(kmer_A, kmer_B, 
                                              type = align.type, substitutionMatrix = align.mat, ...)
    } else {
      if (is.na(na.match) | is.na(na.mismatch)) {
        stop("Both DNA base match/mismatch scores need to be specified!")
      }
      if (!is.na(submat)) {
        if (verbose) {
          cat(sprintf(
            "Nucleic acid inputs with %s as substitution matrix...", submat))
        }
      }
      alignment <- pwalign::pairwiseAlignment(kmer_A, kmer_B, type = align.type,
                                              substitutionMatrix = ifelse(is.na(submat), yes = NULL, no = submat))
    }
  } else {
    kmer_A <- Biostrings::AAString(kmer_A)
    kmer_B <- Biostrings::AAString(kmer_B)
    alignment <- pwalign::pairwiseAlignment(kmer_A, kmer_B, 
                                            type = align.type, substitutionMatrix = submat)
  }		
  return(alignment)
}
