#' Convert predictions to data.frame
#' Return predictions as data.frame
#' @param x results of prediction as produced by \code{\link{predict.ampgram_model}}
#' @return a data.frame with two columns and number of rows corresponding to the
#' number of peptides/proteins in the results of prediction. Columns contain following
#' information:
#' \describe{
#'   \item{seq_name}{Name of an analyzed sequence}
#'   \item{probability}{Probability that a protein/peptide possesses antimicrobial
#'   activity. It assumes values from 0 (non-AMP) to 1 (AMP).}}
#' Row names contain sequence name and decision if a peptide/protein is classified
#' as AMP (\code{TRUE}) or non-AMP (\code{FALSE}). 
#' @export
#' @examples 
#' data(AmpGram_predictions)
#' pred2df(AmpGram_predictions)
pred2df <- function(x) {
  data.frame(seq_name = names(x),
             probability = sapply(x, function(i) i[["single_prot_pred"]]))
}
