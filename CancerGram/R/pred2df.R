#' Convert predictions to data.frame
#' Return predictions as data.frame
#' @param x results of prediction as produced by \code{\link{predict.cancergram_model}}
#' @return a data.frame with two columns and number of rows corresponding to the
#' number of peptides/proteins in the results of prediction. Columns contain following
#' information:
#' \describe{
#'   \item{seq_name}{Name of an analyzed sequence.}
#'   \item{acp}{Probability that a protein/peptide possesses anticancer activity.}
#'   \item{amp}{Probability that a protein/peptide possesses antimicrobial activity.}
#'   \item{neg}{Probability that a protein/peptide do not possesses anticancer 
#'   or antimicrobial activity.}
#'   \item{decision}{Result of the prediction.}
#'   }
#' @export
#' @examples 
#' data(CancerGram_predictions)
#' pred2df(CancerGram_predictions)
pred2df <- function(x) {
  get_decision(cbind(data.frame(seq_name = names(x)),
                     do.call(rbind, 
                             lapply(names(x), function(ith_prot) {
                               x[[ith_prot]][["single_prot_pred"]]
                             })
                     )
  )
  )
}
