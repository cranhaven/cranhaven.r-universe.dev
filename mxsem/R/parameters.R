#' parameters
#'
#' Returns the parameter estimates of an mxModel. Wrapper
#' for omxGetParameters
#' @param mxMod mxModel object
#' @returns vector with parameter estimates
#' @export
parameters <- function(mxMod){
  return(omxGetParameters(mxMod))
}
