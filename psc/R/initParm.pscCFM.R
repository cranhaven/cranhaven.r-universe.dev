#' Fucntion for estimating initial parameter values
#'
#' @param CFM A counter-factual model
#' @param DC_clean a cleaned dataset obsect obtained using dataComb.flexsurvreg
#' @param trt An optional additional vector denoting treatment allocations for multiple treatment comparisons.  Defaults to 'NULL'
#' @details
#' This function takes the liklihood for a 'flexsurvreg' model and uses 'optim'
#'   to fit the likelihood.
#' @return an 'optim' output giving the parameter values to be supplied as a
#'   starting value for the mcmc routine.
#' @export
#'
initParm.pscCFM <- function(CFM,DC_clean,trt=NULL){
  if("flexsurvreg"%in%CFM$mod_class) ret <- initParm.flexsurvreg(CFM,DC_clean,trt=trt)
  if("glm"%in%CFM$mod_class) ret <- initParm.glm(CFM,DC_clean,trt=trt)
  return(ret)
}

