#' Compute point estimates for parameters from posterior samples
#'
#' @export
#'
#' 
#' @param object stFit object containing posterior samples for model
#' @param burn number of posterior samples to reject before computing estimates
#' @param fun function for computing point estimates
#' @param ... S3 generic/method consistency
#'  
#' @examples
#' 
#' data("coprecip.fit")
#' coef(coprecip.fit, burn = 50)
#' 

coef.stFit = function(object, burn = 1, fun = mean, ...) {
  stFit = object
  lapply(stFit$parameters$samples, function(s) {
    apply(as.matrix(s[-(1:burn),]), 2, fun)
  })
}