#' Predict antimicrobial peptides
#'
#' Recognizes antimicrobial peptides using the AmpGram algorithm.
#' @param object \code{ampgram_model} object.
#' @param newdata \code{list} of sequences (for example as given by
#' \code{\link[biogram]{read_fasta}} or \code{\link{read_txt}}).
#' @param ... further arguments passed to or from other methods.
#' @return \code{list} of objects of class \code{single_ampgram_pred}. Each object 
#' of this class contains analyzed sequence, values of predictions for 10-mers and 
#' result of the prediction for the whole peptide/protein.
#' @export
#' @details AmpGram requires the external package, AmpGramModel, which 
#' contains models necessary to perform the prediction. The model 
#' can be installed using \code{\link{install_AmpGramModel}}.
#' 
#' Predictions for each protein are stored in objects of class 
#' \code{single_ampgram_pred}. It consists of three elements:
#' \describe{
#'   \item{seq}{Character vector of amino acid sequence of an analyzed peptide/protein}
#'   \item{all_mers_pred}{Numeric vector of predictions for each 10-mer (subsequence
#'   of 10 amino acids) of a sequence. Prediction value indicates probability that
#'   a 10-mer possesses antimicrobial activity and ranges from 0 (non-AMP) to 1 
#'   (AMP).}
#'   \item{single_prot_pred}{Named numeric vector of a single prediction value for
#'   a whole peptide/protein. Its value corresponds to the probability that a
#'   peptide/protein exhibits antimicrobial activity. It assumes name \code{TRUE} 
#'   if probability is equal or greater than 0.5, i.e. peptide/protein is classified
#'   as antimicrobial (AMP), and \code{FALSE} if probability is less that 0.5,
#'   i.e. peptide/protein is classified as non-antimicrobial (non-AMP).}
#' }
#' @importFrom biogram binarize decode_ngrams
#' @importFrom pbapply pblapply
#' @importFrom ranger ranger
#' @importFrom stats predict
#' @importFrom stringi stri_count

predict.ampgram_model <- function(object, newdata, ...) {
  require_AmpGramModel()
  
  ngrams <- object[["imp_features"]]
    
  decoded_ngrams <- gsub(pattern = "_", replacement = ".", 
                         x = decode_ngrams(ngrams), fixed = TRUE)
  
  
  all_preds <- pblapply(newdata, function(ith_seq) {
    ngram_count <- find_ngrams(seq = ith_seq, decoded_ngrams = decoded_ngrams)
    colnames(ngram_count) <- ngrams
    all_mers_pred <- predict(object[["rf_mers"]], ngram_count)[["predictions"]][, 2]
    single_prot_pred <- predict(object[["rf_peptides"]], 
                                calculate_statistics(all_mers_pred))[["predictions"]][, 2]
    res <- list(seq = ith_seq,
                all_mers_pred = all_mers_pred,
                single_prot_pred = single_prot_pred)
    
    class(res) <- "single_ampgram_pred"
    
    res
  })
  
  if(is.null(names(all_preds))) 
    names(all_preds) <- paste0("seq", 1L:length(all_preds))
  
  all_preds
}


# data(AmpGram_model)
# sample_seq <- list(seq1 = c("F", "E", "N", "C", "N", "I", "T", "M", "G", "N", "M", "V", 
#                             "R", "H", "I", "R", "W", "Y", "R", "D", "R", "Q", "K", "G", "D", 
#                             "Y", "W", "W", "Y", "T", "I", "K", "Y", "S", "M", "A", "M", "I", 
#                             "A", "C", "N", "I", "N", "V", "T", "I", "N", "Q", "C", "V"),
#                    seq2 = c("Q", "Y", "T", "S", "I", "M", "F", "L", "T", "A", "G", "H", 
#                             "L", "A", "P", "W", "D", "R", "W", "C", "R", "S", "L", "T", "T", 
#                             "W", "F", "G", "A", "P", "S", "A", "T", "Y", "P", "F", "F", "W", 
#                             "E", "P", "E", "D", "I", "I", "I", "K", "P", "N", "T", "A"))
# predict(AmpGram_model, sample_seq)
