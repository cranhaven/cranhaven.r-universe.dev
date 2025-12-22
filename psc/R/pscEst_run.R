#' Running the Bayesian MCMC routine
#' A procedure which runs the MCMC estimation routine
#'
#' @param pscOb an pscOb object which has been passed through pscData() and
#' init() functions
#' @param nsim the number of MCMC simulations to run
#' @param nchain Number of chains to use for analysis
#' @return An updated set of attributes for the pscOb which includes
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' pscOb <- pscData(gemCFM,e4_data)
#' pscOb <- init(pscOb)
#' pscOb <- pscEst_start(pscOb,nsim=1000,nchain=2)
#' pscOb <- pscEst_run(pscOb,nsim=1000,nchain=2)
#' @importFrom parallel mclapply
#' @export
pscEst_run <- function(pscOb,nsim,nchain){

  if(nchain==1|.Platform$OS.type=="windows"){
    res <- pscEst_samp(pscOb,nsim)
  }

  if(nchain>1&.Platform$OS.type!="windows"){
        res <- mclapply(1:nchain,mc.cores=pscOb$ncores,
                    function(x) pscEst_samp(pscOb=pscOb,nsim=nsim))
  }

  ### Adding results
  pscOb$draws <- res
  pscOb
}
