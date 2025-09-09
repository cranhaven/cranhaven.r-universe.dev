#' Function for estimating initial parameter values  'flexsurvreg'
#'
#' A function which performs the Bayesian MCMC estimation procedure for
#' estimating the efficacy parameter (\eqn{\beta}) using personalised sunthetic
#' controls methodology.
#'
#' @param CFM a model object supplied to pscfit
#' @param DC_clean a cleaned dataset ontained using dataComb().
#' @param nsim the number of MCMC simulations to run
#' @param start the stating value for
#' @param start.se the stating value fo
#' @param trt an optional vector denoting treatment allocations where multiple
#'     treatment comparisons are being made
#' @details An MCMC routine for fitting a psc model
#' @return A matrix containing the draws form the posterior distribution
#' @import utils
pscEst.pscCFM <- function(CFM,DC_clean,nsim, start, start.se,trt=NULL){
  if("flexsurvreg"%in%CFM$mod_class) ret <- pscEst.flexsurvreg(CFM,DC_clean,nsim, start, start.se,trt=trt)
  if("glm"%in%CFM$mod_class) ret <- pscEst.glm(CFM,DC_clean,nsim, start, start.se,trt=trt)
  return(ret)
}
