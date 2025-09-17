#' Fit a spatially mean-zero spatial Gaussian process model
#'
#' Uses a Gibbs sampler to estimate the parameters of a Matern covariance 
#' function used to model observations from a Gaussian process with mean 0.
#' 
#' @import Rcpp
#' @importFrom stats dist
#' 
#' @export
#'
#' @useDynLib bisque, .registration = TRUE
#' 
#' @param x Observation of a spatial Gaussian random field, passed as a vector
#' @param coords Spatial coordinates of the observation
#' @param nSamples (thinned) number of MCMC samples to generate
#' @param inits list of initial parameters for the MCMC chain
#' @param thin thinning to be used within the returned MCMC samples
#' @param rw.initsd initial standard devaition for random walk proposals.  this 
#'   parameter will be adaptively tuned during sampling
#' @param C scale factor used during tuning of the random walk proposal s.d.
#' @param alpha target acceptance rate for which the random walk proposals 
#'   should optimize
#' @param priors parameters to specify the prior distributions for the model
#' 
#' @example examples/spatial.R
#' 

sFit = function(x, coords, nSamples, thin=1, rw.initsd=.1, inits = list(),
                C=1, alpha=.44, priors = list(sigmasq = list(a=2, b=1), 
                rho = list(L=0, U=1), nu = list(L=0, U=1))) {
  
  d = as.matrix(dist(coords))
    
  if(is.null(inits)) {
    inits = list()
  }
  
  res <- .Call(`t_sfit`, as.double(x), as.matrix(d), as.double(priors$sigmasq$a),
            as.double(priors$sigmasq$b), as.double(priors$rho$L),
            as.double(priors$rho$U), as.double(priors$nu$L),
            as.double(priors$nu$U), as.integer(nSamples), as.integer(thin),
            as.double(rw.initsd), as.list(inits), as.double(C), 
            as.double(alpha))
  
  
  reslist = list(
    parameters = list(samples = res),
    priors = priors,
    coords = coords
  )
  
  class(reslist) = 'sFit'
  
  reslist
}