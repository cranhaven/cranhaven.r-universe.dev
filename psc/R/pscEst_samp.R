#' Starting conditions for Bayesian MCMC estimation procedures in 'pscfit'
#' A procedure which runs the sampling process for MCMC estimation
#'
#' @param pscOb an pscOb object which has been passed through pscData() and
#' init() functions
#' @param nsim the number of MCMC simulations to run
#' @return An updated set of attributes for the pscOb which includes
#' @import utils
pscEst_samp <- function(pscOb,nsim){

  draws <- pscOb$draws;draws
  ### Update
  pb <- txtProgressBar(min = 0, max = nsim, style = 3)

  for(i in 2:nsim){
    setTxtProgressBar(pb, i)
    newd <- pscEst_update(i,draws,pscOb)
    draws[i,] <- newd
  }

  data.frame(draws)
}
