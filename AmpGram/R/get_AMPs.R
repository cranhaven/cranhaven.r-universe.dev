#' Get putative antimicrobial peptides
#' 
#' Function gets sequences recognized as antimicrobial peptides and returns as data.frame. 
#' @param x AmpGram predictions for a single protein
#' @return a data.frame with sequences recognized as antimicrobial peptides (AMPs). 
#' It consists of two columns:
#' \describe{
#'   \item{putative_AMP}{amino acid sequence of a 10-mer (subsequence of an analyzed peptide)
#'   predicted as AMP.}
#'   \item{prob}{Probability with which a 10-mer is recognized as AMP.}}
#' @export
#' @examples 
#' data(AmpGram_predictions)
#' get_AMPs(AmpGram_predictions[[2]])
get_AMPs <- function(x) {
  tenmer_start <- 1L:length(x[["all_mers_pred"]])
  only_AMP_start <- tenmer_start[x[["all_mers_pred"]] > 0.5]
  only_AMP_end <- only_AMP_start + 9
  data.frame(putative_AMP = sapply(1L:length(only_AMP_start), function(ith_pos) 
    paste0(x[["seq"]][only_AMP_start[ith_pos]:only_AMP_end[ith_pos]], collapse = "")
  ), prob = x[["all_mers_pred"]][x[["all_mers_pred"]] > 0.5])
}
