#' Compute effective range for Matern correlation to drop to a specified level
#'
#' The effective range for an isotropic spatial correlation function is 
#' commonly defined to be the distance beyond which the correlation becomes 
#' small, typically below .05.  Given range and smoothness parameters for a 
#' Matern covariance function, this function numerically searches for this 
#' distance.  Note that the scale is not important for this calculation.
#'
#' @export
#'
#' @importFrom stats optim
#' 
#' @param cor Effective correlation to check for
#' @param range Matern range parameter.  Controls the decay of pointwise 
#'        correlations as a function of distance.
#' @param smoothness Matern smoothness parameter.  Controls the number of 
#'        process derivatives.
#'
#' @examples 
#' 
#' # effective range for exponential covariance function with range = 1,
#' # which is theoretically known to equal -ln(.05)
#' maternEffectiveRange(cor = .05, range = 1, smoothness = .5)
#' 

maternEffectiveRange = function(cor = .05, range = 1, smoothness = .5 ) {
  optim(1, function(d) {
    abs( cor - 
           maternArray(d, scale = 1, range = range, smoothness = smoothness) )
  })$par
}