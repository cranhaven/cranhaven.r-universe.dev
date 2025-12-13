
#' Variable Importance for pmforest
#'
#' See \code{\link[partykit]{varimp.cforest}}.
#'
#' @param object DESCRIPTION.
#' @param nperm the number of permutations performed.
#' @param OOB a logical determining whether the importance is computed from the 
#' out-of-bag sample or the learning sample (not suggested).
#' @param risk the risk to be evaluated. By default the objective function 
#' (e.g. log-Likelihood) is used.
#' @param conditional a logical determining whether unconditional or conditional 
#' computation of the importance is performed.
#' @param threshold the value of the test statistic or 1 - p-value of the 
#' association between the variable of interest and a covariate that must be 
#' exceeded inorder to include the covariate in the conditioning scheme for the 
#' variable of interest (only relevant if conditional = TRUE).
#' @param ... passed on to \code{\link{objfun}}.
#'
#' @return A vector of 'mean decrease in accuracy' importance scores.
#' @export
varimp.pmforest <- function(object, nperm = 1L, OOB = TRUE,
                            risk = function(x, ...) - objfun(x, sum = TRUE, ...), 
                            conditional = FALSE, threshold = .2, ...) {
  
  varimp.cforest(object = object, nperm = nperm, OOB = OOB, risk = risk, 
                 conditional = conditional, threshold = threshold, ...)
  
}
