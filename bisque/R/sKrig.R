#' Draw posterior predictive samples from a spatial Gaussian process model
#'
#' @import Rcpp
#' @import foreach
#' @importFrom itertools ichunk
#' @importFrom stats dist
#'
#' @export
#' 
#' @param x Observation of a spatial Gaussian random field, passed as a vector
#' @param sFit posterior samples of model parameters; output from 
#'   bisque::sFit
#' @param coords.krig Spatial coordinates at which the field should be 
#'   interpolated
#' @param coords Spatial coordinates at which observations are available
#' @param burn number of posterior samples to discard from sFit before sampling
#' @param ncores Kriging is done via composition sampling, which may be done in
#'   parallel.  \code{ncores} specifies the number of cores over which sampling 
#'   is done.  If \code{ncores>1}, bisque::sKrig assumes that a parallel 
#'   backend suitable for use with the foreach package is already registered.
#'
#' @useDynLib bisque, .registration = TRUE
#' 
#' @example examples/spatial.R
#' 

sKrig = function(x, sFit, coords.krig, coords = sFit$coords, burn = 0,
                 ncores = 1) {
  
  if(burn > 0) {
    sFit$parameters$samples$sigmasq = sFit$parameters$samples$sigmasq[-(1:burn)]
    sFit$parameters$samples$rho = sFit$parameters$samples$rho[-(1:burn)]
    sFit$parameters$samples$nu = sFit$parameters$samples$nu[-(1:burn)]
  }
  
  d = as.matrix(dist(rbind(as.matrix(coords.krig), as.matrix(coords))))
  
  n0 = nrow(coords.krig)
  
  d00 = d[1:n0, 1:n0]
  d01 = d[1:n0, -(1:n0)]
  d11 = d[-(1:n0), -(1:n0)]
  
  # make looping more efficient
  mcoptions = list(preschedule=FALSE)
  
  # estimate chunksize that will minimize number of function calls
  nSamples = length(sFit$parameters$samples$sigmasq)
  chunkSize = ceiling((nSamples+1)/ncores)
  
  op = ifelse(ncores>1, `%dopar%`, `%do%`)
  
  res = op(foreach(inds = ichunk(1:nSamples, chunkSize = chunkSize, 
                              mode = 'numeric'), 
                .combine = 'rbind', .options.multicore = mcoptions, 
                .packages = 'Rcpp'), {
    
    .Call(`t_spredict`, as.numeric(x), as.matrix(d00), as.matrix(d01),
          as.matrix(d11), 
          as.numeric(sFit$parameters$samples$sigmasq[inds]),
          as.numeric(sFit$parameters$samples$rho[inds]),
          as.numeric(sFit$parameters$samples$nu[inds]))$x0
  })
  
  reslist = list(
    samples = res,
    coords.krig = coords.krig
  )
  
  class(reslist) = 'sKrig'
  
  reslist
}
