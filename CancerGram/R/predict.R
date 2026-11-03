#' Predict anticancer peptides
#'
#' Recognizes anticancer peptides using the CancerGram algorithm.
#' @param object \code{cancergram_model} object.
#' @param newdata \code{list} of sequences (for example as given by
#' \code{\link[biogram]{read_fasta}} or \code{\link{read_txt}}).
#' @param ... further arguments passed to or from other methods.
#' @return \code{list} of objects of class \code{single_cancergram_pred}. 
#' Each object of this class contains analyzed sequence, values of predictions 
#' for 5-mers and result of the prediction for the whole peptide/protein.
#' @export
#' @details CancerGram requires the external package, CancerGramModel, which 
#' contains models necessary to perform the prediction. The model 
#' can be installed using \code{\link{install_CancerGramModel}}.
#' 
#' Predictions for each protein are stored in objects of class 
#' \code{single_cancergram_pred}. It consists of three elements:
#' \describe{
#'   \item{seq}{Character vector of amino acid sequence of an analyzed peptide/protein}
#'   \item{all_mers_pred}{Matrix of predictions for each 5-mer (subsequence
#'   of 5 amino acids) of a sequence. Each row corresponds to one mer and columns
#'   to predicted classes (ACP, AMP or negative). Prediction value indicates probability 
#'   that a 5-mer possesses anticancer activity (acp), antimicrobial activity (amp) or 
#'   none of them (neg).}
#'   \item{single_prot_pred}{One row matrix of a single prediction value for
#'   a whole peptide/protein. Its value corresponds to the probability that a
#'   peptide/protein exhibits anticancer activity, antimicrobial activity or none
#'   of them.}
#' }
#' @importFrom biogram binarize decode_ngrams
#' @importFrom pbapply pblapply
#' @importFrom ranger ranger
#' @importFrom stats predict
#' @importFrom stringi stri_count

predict.cancergram_model <- function(object, newdata, ...) {
  require_CancerGramModel()
  
  ngrams <- object[["imp_features"]]
    
  decoded_ngrams <- gsub(pattern = "_", replacement = ".", 
                         x = decode_ngrams(ngrams), fixed = TRUE)
  
  all_preds <- pblapply(newdata, function(ith_seq) {
    ngram_count <- find_ngrams(seq = ith_seq, decoded_ngrams = decoded_ngrams)
    colnames(ngram_count) <- ngrams
    all_mers_pred <- predict(object[["rf_mers"]], ngram_count)[["predictions"]]
    single_prot_pred <- predict(object[["rf_peptides"]], 
                                calculate_statistics_mc(all_mers_pred, c("acp", "amp", "neg")))[["predictions"]]
    res <- list(seq = ith_seq,
                all_mers_pred = all_mers_pred,
                single_prot_pred = single_prot_pred)
    
    class(res) <- "single_cancergram_pred"
    
    res
  })
  
  if(is.null(names(all_preds))) 
    names(all_preds) <- paste0("seq", 1L:length(all_preds))
  
  all_preds
}

