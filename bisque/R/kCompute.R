#' Use sparse grid quadrature techniques to integrate (unnormalized) densities
#'
#' This function integrates (unnormalized) densities and may be used to compute
#' integration constants for unnormalized densities, or to marginalize a 
#' joint density, for example.
#' 
#' @export
#' 
#' @importFrom stats optim
#' 
#' @param f (Unnormalized) density to integrate.
#'   the function \eqn{f} should include an argument \code{log}, 
#'   which returns \eqn{log(f(x))}.
#' @param init Initial guess for the density's mode
#' @param maxit maximum number of iterations \code{optim} should use in 
#'   searching for the density's mode
#' @param method method to be used to search for the density's mode
#' @param level accuracy level (typically number of grid points for the 
#'   underlying 1D quadrature rule) [description from mvQuad::createNIGrid]
#' @param log TRUE to return log of integration constant
#' @param link character vector that specifies transformations used during 
#'   optimization and integration of f(theta2 | X).  while theta2 may be 
#'   defined on arbitrary support, \code{wtdMix} performs optimization and 
#'   integration of theta2 on an unconstrained support.  the \code{link} 
#'   vector describes the transformations that must be applied to each 
#'   element of theta2.  Jacobian functions for the transformations will 
#'   automatically be added to the optimization and integration routines.
#'   currently supported link functions are 'log', 'logit', and 'identity'.
#' @param linkparams Optional list of additional parameters  for link functions.
#'   For example, the logit function can be extended to allow mappings to any 
#'   closed interval.  There should be one list entry for each link function.  
#'   Specify NA if no additional arguments are passed.
#' @param quadError TRUE if integration nodes and weight should be computed for
#'  the \code{level-1} integration grid, so that quadrature approximation
#'  error can be estimated.
#' @param ... additional arguments to pass to \code{f}
#' 
#' @examples
#' kCompute(dgamma, init = 1, shape=2, link='log', level = 5)
#' 
kCompute = function(f, init, method = 'BFGS', maxit=1e4, level = 2, log = FALSE,
                    link = NULL, linkparams = NULL, quadError = FALSE, ...) {
  
  # default is identity links
  if(is.null(link)) {
    link = rep('identity', length(init))
  }
  
  # default is no additional link parameters
  if(is.null(linkparams)) {
    linkparams = as.list(rep(NA, length(init)))
  }
  
  # find the density's mode
  mode = optim(par = tx(init, link, linkparams), fn = function(par, ...) {
    f(itx(par, link, linkparams), log = TRUE, ...) + 
      sum(logjac(par, link, linkparams))
  }, method = method, control = list(fnscale = -1, maxit=maxit), 
  hessian = TRUE, ...)
  
  # warn if optimization failed
  if(mode$convergence != 0) {
    warning('Mode not found.')
  }
  
  # build integration grid
  grid = createLocScaleGrid(mu = mode$par, prec = -mode$hessian, level = level,
                            quadError = quadError)
  
  # evaluate the unnormalized log-density at integration points
  lnf = apply(grid$nodes, 1, function(x){
    f(itx(x, link, linkparams), log = TRUE, ...) + 
      sum(logjac(x, link, linkparams)) })
  lnf = lnf - grid$d
  
  # initialize return with scaled integration constant
  lnk = -mean(lnf)
  kC = sum(exp(lnf + lnk) * grid$weights)
  lnC = log(kC) - lnk
  
  r = ifelse(log, lnC, exp(lnC))
  
  if(quadError) { 
    lnf = lnf[grid$errorNodes$inds]
    lnk = -mean(lnf)
    kC = sum(exp(lnf + lnk) * grid$errorNodes$weights)
    lnC.l = log(kC) - lnk
  }
  
  if(quadError) {
    r.l = ifelse(log, lnC.l, exp(lnC.l))
    list(res = r,
         res.coarse = r.l,
         rel.err.bound = (r.l - r)/r * 100)
  } else {
    r
  }
}