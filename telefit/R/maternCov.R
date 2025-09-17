#' Matern covariance
#'
#' This function evaluates the Matern covariance function for the elements of 
#' a (potentially non-square) spatial distance matrix
#'
#' @useDynLib telefit, .registration = TRUE
#' 
#' @export
#'
#' @param d A numeric vector or matrix of distances at which the Matern 
#'        correlation function should be evaluated.
#' @param scale Scales correlations to covariances.
#' @param range Matern range parameter.  Controls the decay of pointwise 
#'        correlations as a function of distance.
#' @param smoothness Matern smoothness parameter.  Controls the number of 
#'        process derivatives.
#' @param nugget Spatial covariance nugget.  
#' 
#' @examples 
#' data("coprecip")
#' attach(coprecip)
#' 
#' # compute spatial covariance matrix for an exponential covariance function
#' # using Colorado coordinates
#' Sigma = maternCov(fields::rdist.earth(coords.s), scale = 1, range = 250,
#'   smoothness = .5, nugget = 0)
#' 

maternCov = function(d, scale = 1, range = 1, smoothness = .5, nugget = 0) {
  .Call(`_telefit_r_maternCov`, as.matrix(d), scale, range, smoothness, 
        nugget)
}