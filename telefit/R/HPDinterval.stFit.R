#' Compute Highest posterior density intervals from posterior samples
#'
#' @export HPDinterval.stFit
#'
#' @importFrom coda HPDinterval mcmc
#' 
#' @param stFit stFit object containing posterior samples for model
#' @param burn number of posterior samples to reject before computing estimates
#' @param prob The target probability content of the intervals
#'  
#' @examples
#' 
#' data("coprecip.fit")
#' HPDinterval.stFit(coprecip.fit, burn = 50)
#' 

HPDinterval.stFit = function(stFit, burn = 1, prob = .95) {
  res = lapply(stFit$parameters$samples, function(s) {
    r = s[-(1:burn),]
    if(length(r)>1) {
      HPDinterval(mcmc(as.matrix(r)))
    } else {
      NULL
    }
  })
  
  # add names for the betas
  if(!is.null(stFit$parameters$beta.names)) {
    rownames(res$beta) = stFit$parameters$beta.names
  }
  
  res
}