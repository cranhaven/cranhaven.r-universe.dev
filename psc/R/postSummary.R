#' Posterior Summary
#'
#' A function that provides a summary of the posterior distributions obtained
#' from a pscEst() procedures
#'
#' @param pscOb a pscOb function which has passed through pscEst()
#' @param thin a thin to be applied to the posterior distributions
#' @param burn a burnin to ba applied to the posterior distribution
#' @param par the parameter to be summarised - defaults to 'beta' to summarise
#' all 'beta' parameters in the posterior distribution
#'
#' @details This function makes use of the 'posterior' package to pull together
#' each of the 'draw' matrices included in the psc object and produce posterior
#' summaries
#' @return Returns a summary of a 'psc' object including details on the original
#' Counter Factual Model, a summary of the Data Cohort, the predicted responses
#' from the CFM and details on the model fit.
#' @importFrom posterior summarise_draws as_draws thin_draws
#' @examples
#' e4_data <- psc::e4_data
#' gemCFM <- psc::gemCFM
#' pscOb <- pscData(gemCFM,e4_data)
#' pscOb <- init(pscOb)
#' pscOb <- pscEst(pscOb)
#' pscOb <- postSummary(pscOb)
#' @export
#'
postSummary <- function(pscOb,thin=2,burn=1000,par="beta"){


  ## Creating draws object
  drs <- as_draws(pscOb$draws)
  nsim <- dim(drs)[1]

  ## Error message based on burn in being too small
  if(burn>=nsim) {
    burn <- nsim/10
    warning(paste("Burn-in smaller than number of simulations and set to",burn))
  }

  ### Getting dimensions of draws
  dim_drs <-   dim(drs);dim_drs

  ### removing burn in
  drs <- drs[-c(1:burn),,]

  # thin draws
  drs <- thin_draws(drs,thin=thin)

  # summary Table
  summConv <- summarise_draws(drs, "rhat", "ess_bulk", "ess_tail","mcse_mean")
  summEst <- summarise_draws(drs, "mean", "sd", "median","quantile2")

  # Selecting only
  summConv <- summConv[grep(par,summConv$variable),]
  summEst <- summEst[grep(par,summEst$variable),]

  ## returning objects
  pscOb$draws <- drs
  pscOb$postFit <- summConv
  pscOb$postEst  <- summEst

  return(pscOb)
}
