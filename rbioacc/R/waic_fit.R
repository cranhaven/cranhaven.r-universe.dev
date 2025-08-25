#' Widely Applicable Information Criterion (WAIC)
#' 
#' Compute WAIC using the \code{waic()} method of the loo package.
#' 
#' @param fit An object of class \code{fitTK}
#' 
#' @return A numeric containing the WAIC
#' 
#' @export
#' 
#' 
waic <- function(fit){
  fitMCMC = as.matrix(rstan::extract(fit[["stanfit"]], permuted = FALSE))
  waicEval <- loo::waic(fitMCMC)
  return(waicEval$estimates[3,1])
}