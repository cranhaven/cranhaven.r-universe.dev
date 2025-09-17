#' Compute expectations via weighted mixtures
#' 
#' Approximates expectations of the form
#' \deqn{E[h(\theta)] = \int h(\theta) f(\theta) d\theta}
#' using a weighted mixture
#' \deqn{E[h(\theta)] \approx \sum_{j=1}^k h(\theta^{(k)}) w_k}
#' 
#' @import foreach
#' @importFrom itertools ichunk
#' 
#' @export
#' 
#' @param h Function for which the expectation should be taken.  The function 
#'   should be defined so it is can be called via \code{f(params, ...)}.
#'   Additional parameters may be passed to \eqn{h} via \code{...}.
#' @param params Matrix in which each row contains parameters at which
#'   \eqn{h} should be evaluated.  The number of rows in \code{params} should 
#'   match the number of mixture components \eqn{k}.
#' @param wts vector of weights for each mixture component
#' @param ncores number of cores over which to evaluate mixture.  this function
#'   assumes a parallel backend is already registered.
#' @param errorNodesWts list with elements \code{inds} and \code{weights} that 
#'   point out which \code{params} get used to compute an approximation of the 
#'   quadrature error.
#' @param ... additional arguments to be passed to \code{h}
#' 
#' @example examples/emixEx.R
#' 
emix = function(h, params, wts, ncores = 1, errorNodesWts = NULL, ...){
  
  if(!is.matrix(params)) {
    params = matrix(params, ncol=1)
  }
  
  op = ifelse(ncores > 1, `%dopar%`, `%do%`)
  
  # approximate expectation by summing over mixtures
  np = nrow(params)
  chunkSize = ceiling(np/ncores)
  res = op(foreach(inds = ichunk(1:np, chunkSize = chunkSize, mode = 'numeric'),
                .combine = rbind), {
    
    # initialize partial posterior mean
    h.theta = 0
    
    # initialize state for quadrature error bound
    h.theta.l = 0
    if(!is.null(errorNodesWts)) { 
      s = which( errorNodesWts$inds >= inds[1] )
      if(length(s)>0) {
        err.ind = min(s)
        next.err.ind = errorNodesWts$inds[err.ind]
      } else {
        next.err.ind = -1
      }
    }   
    
    for(i in inds) {
      
      # build posterior mean estimate
      h.eval = h(as.numeric(params[i,]), ...)
      h.theta = h.theta + h.eval * wts[i]
      
      # build quadrature error bound
      if(!is.null(errorNodesWts)) {
        if(i == next.err.ind) {
          
          h.theta.l = h.theta.l + h.eval * errorNodesWts$weights[err.ind]
          
          err.ind = err.ind + 1
          next.err.ind = ifelse(err.ind <= length(errorNodesWts$inds), 
                                errorNodesWts$inds[err.ind],
                                -1)
        }
      }
      
    }
    
    matrix(c(h.theta, h.theta.l), nrow = 1)
  
  })
  
  # merge results
  h.theta = sum(res[,1])
  h.theta.l = sum(res[,2])
  
  if(!is.null(errorNodesWts)) {
    list( E = h.theta,
          E.coarse = h.theta.l,
          rel.err.bound = (h.theta.l - h.theta)/h.theta * 100)
  } else {
    h.theta
  }
}
