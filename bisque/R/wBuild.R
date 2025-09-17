#' Derive parameters for building integration grids
#'
#' Note: \eqn{w} is defined on the transformed scale, but for convenience 
#' \code{f} is defined on the original scale.
#'
#' @importFrom stats optim
#' 
#' @export
#'
#' @param f function used to derive the weight function \eqn{w}.
#'   \code{f} must be able to be called via \code{f(par, log, ...)}
#' @param init initial guess for mode of \code{f}.
#' @param dim.theta2 \code{wBuild} assumes \code{par} is partitioned such that 
#'   \code{par=c(theta1,theta2)}.  \code{dim.theta2} specifies the size of the 
#'   partition.  The default is to assume that \code{f} is defined without a 
#'   \code{theta1} component.
#' @param approx Style of approximation (i.e., \eqn{w}) to be created from mode 
#'   of \code{f}.
#'   \describe{
#'     \item{\code{'gaussian'}}{Gaussian approximation for \code{theta2} at 
#'       the mode of \code{f}. Assumes \code{f} is proportional to the marginal 
#'       posterior density for \code{theta2}.}
#'     \item{\code{'condgauss'}}{Gaussian approximation for \code{theta2} at 
#'       the mode of \code{f}.  The approximation is conditioned on the value of
#'       the mode for \code{theta1}. Assumes \code{f} is proportional to the 
#'       joint posterior density for \code{theta1,theta2.}}
#'     \item{\code{'condgauss-laplace'}}{Gaussian approximation for 
#'       \code{theta2} at the mode of \code{f}.  The approximation is 
#'       conditioned on a separate laplace approximation of the marginal 
#'       posterior mode for \code{theta1}.  Assumes \code{f} is proportional to 
#'       the joint posterior density for \code{theta1,theta2.}}
#'     \item{\code{'margauss'}}{Gaussian approximation for 
#'       \code{theta2} at the mode of \code{f}.  Assumes \code{f} is 
#'       proportional to the joint posterior density for \code{theta1,theta2.}, 
#'       then uses the marginal mean and covariance from the posterior's 
#'       gaussian approximation.}
#'   }
#' @param link character vector that specifies transformations used during
#'   optimization and integration of \eqn{f(\theta_2 | X)}.  While
#'   \eqn{\theta_2} may be defined on arbitrary support, \code{wtdMix} performs
#'   optimization and integration of \eqn{\theta_2} on an unconstrained support.
#'   The \code{link} vector describes the transformations that must be applied
#'   to each element of \eqn{\theta_2}.  Jacobian functions for the
#'   transformations will automatically be added to the optimization and
#'   integration routines. Currently supported link functions are \code{'log'},
#'   \code{'logit'}, and \code{'identity'}.
#' @param link.params Optional list of additional parameters for link
#'   functions.  For example, the logit function can be extended to allow
#'   mappings to any closed interval.   There should be one list entry for each
#'   link function.  Specify NA if no additional arguments are passed.
#' @param optim.control List of arguments to pass to \code{stat::optim}
#'     when used to find mode of \code{f}.
#'   \describe{
#'     \item{\code{maxit}}{Maximum number of iterations to run \code{optim} 
#'       for.}
#'     \item{\code{method}}{Optimization routine to use with \code{optim}.}
#'   }
#' @param ... additional arguments needed for function evaluation.
#' 
#' @example examples/seals.R
#'
wBuild = function(f, init, dim.theta2 = length(init), approx = 'gaussian',
                  link = rep('identity', length(init)),
                  link.params = rep(list(NA), length(init)),
                  optim.control = list(maxit = 5e3, method = 'BFGS'), ...) {
  
  # check approximation type
  approxs = c('condgauss', 'condgauss-laplace', 'gaussian', 'margauss')
  approx = approxs[pmatch(approx, approxs)]
  if(is.na(approx)) { stop('Invalid approximation type.') }
  
  # get indices of theta2 terms
  st = length(init) - dim.theta2 + 1
  end = length(init)
  theta2.inds = st:end
  
  #
  # determine approximation's mean and covariance parameters
  #
  
  # configure g, the posterior object to optimize
  if(approx %in% c('condgauss', 'gaussian', 'margauss')) { 
    g = f
    g.init = init
    g.link = link
    g.link.params = link.params
  } else if(approx == 'condgauss-laplace') {
    
    g.init = init[theta2.inds]
    g.link = link[theta2.inds]
    g.link.params = link.params[theta2.inds]
    
    # laplace approximation for f(theta1|X)
    lapprox = function(theta1, log = TRUE) {
      
      # find mode of f(theta2|theta1, X)
      o = optim(par = tx(g.init, g.link, g.link.params), 
        fn = function(par) {
          f(c(theta1, itx(par, g.link, g.link.params)), 
            log = TRUE, ...) + 
          sum(logjac(par, g.link, g.link.params))
          }, method = optim.control$method, hessian = TRUE,
        control = list(fnscale = -1, maxit = optim.control$maxit)
      )
      
      if(o$convergence != 0) { 
        warning('Laplace approximation may have failed.') 
      }
      
      # Tierney-Kadane approximation
      res = f(c(theta1, itx(o$par, g.link, g.link.params)), log = TRUE, ...) - 
        .5 * as.numeric(determinant(-o$hessian)$modulus) + 
        sum(logjac(o$par, g.link, g.link.params))
      
      if(log) { res } else { exp(res) }
    }
    
    # approximate posterior mode for theta1
    opt.laplace = optim(
      par = tx(init[-theta2.inds], link[-theta2.inds], 
               link.params[-theta2.inds]), 
      fn = function(par) {
        lapprox(itx(par, link[-theta2.inds], link.params[-theta2.inds]), 
                log = TRUE) + 
          sum(logjac(par, link[-theta2.inds], link.params[-theta2.inds]))
      }, 
      method = optim.control$method, hessian = FALSE, 
      control = list(fnscale = -1, maxit = optim.control$maxit)
    )
    
    if(opt.laplace$convergence != 0) { 
      warning('Mode for Laplace approximation may not have been found.') 
    }
    
    laplace.mode = itx(opt.laplace$par, link[-theta2.inds], 
                       link.params[-theta2.inds])
    
    g = function(theta2, log = TRUE, ...) {
      f(c(laplace.mode, theta2), log, ...)
    }
  }
  
  # optimize posterior object (e.g., f(theta1, theta2|X), etc.)
  opt = optim(par = tx(g.init, g.link, g.link.params), fn = function(par) {
    g(itx(par, g.link, g.link.params), log = TRUE, ...) + 
      sum(logjac(par, g.link, g.link.params))
  }, method = optim.control$method, hessian = TRUE, 
  control = list(fnscale = -1, maxit = optim.control$maxit))
  
  if(opt$convergence != 0) { 
    warning('Gaussian approximation may not have converged.') 
  }
  
  # extract parameters
  if(approx == 'condgauss') {
    mode = opt$par[theta2.inds]
    prec = - opt$hessian[theta2.inds, theta2.inds]
    g.link = g.link[theta2.inds]
    g.link.params = g.link.params[theta2.inds]
  } else if(approx %in% c('condgauss-laplace', 'gaussian')) {
    mode = opt$par
    prec = - opt$hessian
  } else if(approx == 'margauss') {
    mode = opt$par[theta2.inds]
    g.link = g.link[theta2.inds]
    g.link.params = g.link.params[theta2.inds]
    
    L = t(chol(-opt$hessian[-theta2.inds, -theta2.inds]))
    M = backsolve(L, -opt$hessian[-theta2.inds, theta2.inds])
    prec = - opt$hessian[theta2.inds, theta2.inds] - t(M)%*%M
  }
    
  # assemble results
  res = list(
    params = list(mode = mode, prec = prec),
    gridfn = function(level = 2, quadError = TRUE) { 
      createLocScaleGrid(mu = mode, prec = prec, level = level, 
                         quadError = quadError)
    },
    link = g.link,
    link.params = g.link.params,
    opt = opt
  )
  
  if(approx == 'condgauss-laplace') { res$opt.laplace = opt.laplace }
  
  class(res) = 'wBuild'
  res
}