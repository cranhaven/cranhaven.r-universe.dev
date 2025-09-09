#' Fucntion for estimating initial parameter values  'flexsurvreg'
#'
#' A generic function for estimating the initial parameters for estimation as part
#' of the pscfti.R function.  Parameter estimates are obtained using standard
#' optomisation methods provided by the 'optim' set of functions.  For a single
#' parameter a Brent method is applied.  For mutliple treatment comparisons the
#' 'BFGS' method is applied.
#'
#' @param CFM A counter-factual model
#' @param DC_clean a cleaned dataset obsect obtained using dataComb.flexsurvreg
#' @param trt An optional additional vector denoting treatment allocations for multiple treatment comparisons.  Defaults to 'NULL'
#' @details
#' This function takes the liklihood for a 'flexsurvreg' model and uses 'optim'
#'   to fit the likelihood.
#' @return an 'optim' output giving the parameter values to be supplied as a
#'   starting value for the mcmc routine.
initParm <- function(CFM, DC_clean, trt){
  UseMethod("initParm")}
