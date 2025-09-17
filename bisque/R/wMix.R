#' Construct a weighted mixture object
#'
#' For a Bayesian model
#' \deqn{ X ~ f(X | \theta_1, \theta_2)}
#' \deqn{ (\theta_1, \theta_2) ~ f(\theta_1, \theta_2),}
#' the marginal  posterior \eqn{f(\theta_1 | X)} distribution can be
#' approximated via weighted mixtures via
#' \deqn{ f(\theta_1 | X) \approx \sum_{j=1}^K f(\theta_1 | X, \theta_2) w_j }
#' where \eqn{w_j} is based on \eqn{f(\theta_2^{(j)} | X)} and weights
#' \eqn{\tilde w_j}, where \eqn{\theta_2^{(j)}} and \eqn{\tilde w_j} are
#' nodes and weights for a sparse-grid quadrature integration scheme.
#' The quadrature rule is developed by finding the posterior mode of
#' \eqn{f(\theta_2|X)}, after transforming \eqn{\theta_2} to an unconstrained
#' support.  For best results, \eqn{\theta_2} should be a continuous random
#' variable, or be able to be approximated by one.
#'
#' @import foreach
#' @importFrom itertools ichunk
#' @importFrom stats optim
#'
#' @export
#'
#' @param f1 evaluates \eqn{f(\theta_1 | X, \theta_2)}.  \code{f1} must be able
#'   to be called via \code{f1(theta1, params, log, ...)}.
#'   \describe{
#'     \item{\code{theta1}}{a matrix of parameters at which to evaluate
#'       \eqn{f(\theta_1 | X, \theta_2)}. each row should be one set of values
#'       at which the density should be evaluated}
#'     \item{params}{a vector of parameters needed to evaluate
#'       \eqn{f(\theta_1 | X, \theta_2)}.  In most cases \code{params} will
#'       equal \eqn{theta_2}, but in some cases, \eqn{f(\theta_1 | X, \theta_2)}
#'       depends on functions of \eqn{\theta_2}, which can be pre-evaluated
#'       as the weighted mixture approximation is constructed.}
#'     \item{log}{TRUE to return \eqn{ln(f(\theta_1 | X, \theta_2))}}
#'     \item{...}{additional arguments needed for function evaluation}
#'   }
#' @param f2 evaluates \eqn{f(theta_2 | X)}.  \code{f2} must be able to be
#'   called via \code{f2(theta2, log, ...)}.
#' @param w \code{wBuild} object created by \code{wBuild} function.  \code{w} 
#'   contains posterior mode of \eqn{f(\theta_2| X)} and wrapper functions to 
#'   generate quadrature grid.
#' @param f1.precompute function that pre-computes parameters for evaluating
#'   \eqn{f(\theta_1 | X, \theta_2)}.  \code{f1.precompute} must be able to
#'   be called via \code{f1.precompute(theta2, ...)} and return the argument
#'   \code{params} for the function \code{f1}.
#' @param spec Specification of whether \code{f1} and \code{f2} are known 
#'  exactly, or need numerical approximation to determine integration constants.
#'  \code{'ff'} if both functions are known, \code{'gg'} if \code{f1} is 
#'  proportional to the full conditional distribution 
#'  \eqn{f(\theta_1|\theta_2,X)}, but needs the integration constant computed,
#'  and if the marginal posterior \eqn{f(theta_2|X)} is equal to \code{f2} times
#'  the integration constant that needs to be numerically approximated.
#' @param level accuracy level of the numerical approximation
#'   (typically number of grid points for the
#'   underlying 1D quadrature rule) [description from mvQuad::createNIGrid]
#' @param c.int If \code{spec=='gg'}, then \code{c.int} specifies the function
#'  that can be integrated in order to yield the missing integration constant.
#' @param c.level accuracy level of the numerical approximation for \code{c.int} 
#'   (typically number of grid points for the
#'   underlying 1D quadrature rule) [description from mvQuad::createNIGrid]
#' @param c.init initial guess for mode of \code{c.int}.
#' @param c.link character vector that specifies transformations used during
#'   optimization and integration of \code{c.int}.  See corresponding 
#'   documentation in \code{wBuild} function for more details.
#' @param c.link.params Optional list of additional parameters for link
#'   functions used with \code{c.int}.   See corresponding 
#'   documentation in \code{wBuild} function for more details.
#' @param c.optim.control Arguments used to find mode of \code{c.int}.   See 
#'   corresponding  documentation in \code{wBuild} function for more details.
#' @param ... Additional arguments to pass to \code{f1}, \code{f1.precompute},
#'   \code{f12}, and \code{f2}.
#' @param ncores number of cores used to parallelize computation of parameters
#'   for \eqn{f(\theta_1 | \theta_2, X)}.
#' @param quadError TRUE if integration nodes and weight should be computed for
#'  the \code{level-1} integration grid, so that quadrature approximation
#'  error can be estimated.
#'
#' @return A list with class \code{wMix}, which contains the following items.
#'   \describe{
#'     \item{\code{f}}{Function for evaluating the posterior density
#'      \eqn{f(\theta_1|X)}.  \code{f} is callable  via
#'      \code{f(theta1, log, ...)}.}
#'     \item{\code{mix}}{A matrix containing the pre-computed parameters for
#'       evaluating the mixture components \eqn{f(\theta_1 | \theta_2, X)}.
#'       Each row of the matrix contains parameters for one of the \eqn{K}
#'       mixture components.}
#'     \item{\code{wts}}{Integration weights for each of the mixture components.
#'       Some of the weights may be negative.}
#'     \item{\code{expectation}}{List containing additional tools for computing
#'       posterior expectations of \eqn{f(\theta_2|X)}.  However, posterior
#'       expectations of \eqn{f(\theta_1|X)} can also be computed when
#'       expectations of \eqn{f(\theta_1|\theta_2, X)} are known.  The elements
#'       of \code{expectation} are
#'       \describe{
#'         \item{\code{Eh}}{Function to compute \eqn{E[h(\theta_2)|X]}.
#'           \code{Eh} is callable via \code{Eh(h, ...)}, where \code{h} is a
#'           function callable via \code{h(theta2, ...)} and \code{...} are
#'           additional arguments to the function.  The function \code{h} is
#'           evaluated at the quadrature nodes \eqn{\theta_2^{(j)}}.}
#'         \item{\code{Eh.precompute}}{Exactly the same idea as \code{Eh}, but
#'           the function \code{h} is evalauted at the quadrature nodes after
#'           being passed through the function \code{f1.precompute}.}
#'         \item{\code{grid}}{The sparse-quadrature integration grid used.
#'           Helpful for seeing the quadrature nodes \eqn{\theta_2^{(j)}}.}
#'         \item{\code{wts}}{The integration weights for approximating the
#'           expectation \eqn{E[h]}.  Note that these integration weights may
#'           differ from the main integration weights for evaluating the
#'           posterior density \eqn{f(\theta_1|X)}.}
#'       }}
#'   }
#'
#' @example examples/seals.R
#'
wMix = function(f1, f2, w, f1.precompute = function(x, ...){x}, spec = 'ff', 
                level = 2, c.int = NULL, c.level = 2, c.init = NULL,
                c.link = rep('identity', length(c.init)),
                c.link.params = rep(list(NA), length(c.init)),
                c.optim.control = list(maxit = 5e3, method = 'BFGS'),
                ncores = 1, quadError = TRUE, ...) {
  
  # determine if intermediate integration constants need to be computed
  spec.split = strsplit(spec, character(0))[[1]]
  f1.cst = ifelse(spec.split[1] == 'g', TRUE, FALSE)
  f2.cst = ifelse(spec.split[2] == 'g', TRUE, FALSE)
  f.cst = any(f1.cst, f2.cst)
  if(f.cst) {
    if(any(is.null(c.int), is.null(c.init))) {
      stop('Must provide c.int AND c.init.')
    }
  }

  # create integration grid
  grid = w$gridfn(level = level, quadError = quadError)

  op = ifelse(ncores > 1, `%dopar%`, `%do%`)

  # precompute parameters and weights for posterior mixture for theta1
  p0 = f1.precompute(itx(grid$nodes[1,], w$link, w$link.params), ...)
  nodes  = nrow(grid$nodes)
  chunkSize = ceiling(nodes/ncores)
  pc = op(foreach(
    inds = ichunk(1:nodes, chunkSize = chunkSize, mode = 'numeric'),
    .combine = mergePars, .export = c('itx', 'logjac', 'kCompute')), {

    # initialize return objects
    mix = matrix(NA, nrow = length(inds), ncol = length(p0))
    wts = numeric(nrow(mix))
    wts.e = numeric(nrow(mix))
    C1 = numeric(nrow(mix))
    nodes.backtransformed = grid$nodes[inds, , drop = FALSE]

    for(i in 1:nrow(mix)) {

      # back-transform parameters
      theta2 = as.numeric(itx(grid$nodes[inds[i],], w$link, w$link.params))

      # compute mixture parameters
      mix[i,] = f1.precompute(theta2, ...)

      # compute base weights (i.e., the weight function ratios)
      wts[i] = f2(theta2, log = TRUE, ...) +
        sum(logjac(grid$nodes[inds[i],], w$link, w$link.params)) -
        grid$d[inds[i]]

      # compute weights for evaluating expectations in secondary analyses
      wts.e[i] = wts[i]

      # as necessary, compute C1(theta2) and update weights
      if(f.cst) {
        C1[i] = kCompute(f = function(theta1, log = TRUE) {
          res = c.int(theta1, theta2, log = log, ...)
          if(log) { res } else { exp(res) }
        }, init = c.init, log = TRUE, level = c.level, link = c.link,
        linkparams = c.link.params, method = c.optim.control$method,
        maxit = c.optim.control$maxit)
        
        if(f2.cst) { wts[i] = wts[i] + C1[i] }
      }

      # store back-transformed integration nodes in grid
      nodes.backtransformed[i,] = theta2
    }

    # package results
    list(mix = mix, wts = wts, wts.e = wts.e, C1 = C1,
         nodes.backtransformed = nodes.backtransformed
    )
  })

  # unwrap results
  mix = pc$mix
  wts = pc$wts
  wts.e = pc$wts.e
  C1 = pc$C1
  grid$nodes = pc$nodes.backtransformed

  # standardize quadError weights before the main quadrature weights
  if(quadError) {
    grid$errorNodes$weights = grid$errorNodes$weights *
      exp(wts[grid$errorNodes$inds]-mean(wts[grid$errorNodes$inds]))
    grid$errorNodes$weights =
      grid$errorNodes$weights / sum(grid$errorNodes$weights)
  }

  # normalize weights
  wts = exp(wts - mean(wts)) * grid$weights
  wts = wts / sum(wts)

  # use C1(theta2) to build a second-layer function for dmix
  if(!f1.cst) { h = f1 }
  else {
    # TODO: Make it safer to pass C1 to the dmix function
    mix = cbind(mix, C1)
    h = function(theta1, params, log, ...) {
      res = f1(theta1, params, log, ...) - params[length(params)]
      if(log) { res } else { exp(res) }
    }
  }

  # build and return weighted marginal posterior
  res = list(
    f = function(theta1, log = FALSE, quadError = FALSE, ...) {
      if(quadError) {
        dmix(x = theta1, f = h, params = mix, wts = wts, log = log,
             errorNodesWts = grid$errorNodes, ...)
      } else {
        dmix(x = theta1, f = h, params = mix, wts = wts, log = log, ...)
      }
    },
    mix = mix,
    wts = wts,
    expectation = list(
      Eh = function(h, ncores = 1, quadError = FALSE, ...) {
        if(quadError) {
          emix(h = h, params = grid$nodes, wts = wts, ncores = ncores,
               errorNodesWts = grid$errorNodes, ...)
        } else {
          emix(h = h, params = grid$nodes, wts = wts, ncores = ncores, ...) }
      },
      Eh.precompute = function(h, ncores = 1, quadError = FALSE, ...) {
        if(quadError) {
          emix(h = h, params = mix, wts = wts, ncores = ncores,
               errorNodesWts = grid$errorNodes, ...)
        } else {
          emix(h = h, params = mix, wts = wts, ncores = ncores, ...) }
      },
      grid = grid
    ),
    ratios = wts.e
  )
  class(res) = 'wMix'
  res
}
