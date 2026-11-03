#' @name CancerGram_predictions
#' @title Prediction of anticancer peptides
#' @description Predictions made with the CancerGram methods.
#' @docType data
#' @format A list of predictions for exemplary sequences. 
#' #' Predictions for each protein are stored in objects of class 
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
#' @keywords datasets
NULL