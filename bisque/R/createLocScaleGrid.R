#' Create a centered and scaled sparse integration grid
#' 
#' Enhances mvQuad::createNIGrid by shifting and scaling a sparse integration
#' grid, and evaluating the weight function at each of the grid nodes.
#' 
#' @export
#' 
#' @importFrom mvQuad createNIGrid
#' @importFrom stats dnorm
#' 
#' @param mu location at which grid should be centered
#' @param prec "precision matrix" associated with the integration grid.  When 
#'   building a sparse integration grid for a density, \code{prec} is often 
#'   the negative of the hessian at the mode.
#' @param level accuracy level.  This is typically number of grid points for the 
#'   underlying 1D quadrature rule.  [description from mvQuad::createNIGrid]
#' @param quadError provide additional information about the grid points and 
#'   integration weights for the quadrature rule with \code{level-1}.  This 
#'   information can facilitate approximating the quadrature error.
#' @param prec.chol Upper-triangular Cholesky decomposition of precision matrix.
#'   
#' @seealso \code{mvQuad::createNIGrid}
#'   
#' @examples 
#' g = createLocScaleGrid(mu = c(1,0), prec = diag(c(1,.5)), level = 2 )
#' 
createLocScaleGrid = function(mu = 0, prec = 1, level = 2, 
                              quadError = FALSE, prec.chol = chol(prec)) {
  
  # determine standardized quadrature points
  grid = createNIGrid(dim = length(mu), level = level, type = 'nHN', 
                      ndConstruction = 'sparse')
  
  if(quadError) {
    # find indices of lower-order integration grid in the higher-order grid
    
    # create lower-order integration grid
    grid.l = createNIGrid(dim = length(mu), level = level - 1, type = 'nHN', 
                          ndConstruction = 'sparse')
    
    if(length(grid.l$weights) == length(grid$weights))
      warning(paste('Quadrature approximation error may be inaccurate;',
                'Approximation error grid may be identital to main grid.'))
    
    # search for the lower-order nodes in the higher-order grid
    grid.l$inds = numeric(length = nrow(grid.l$nodes))
    st = 1
    for(i in 1:nrow(grid.l$nodes)) {
      for(j in st:nrow(grid$nodes)) { 
        if(all(grid.l$nodes[i,] == grid$nodes[j,])) { 
          grid.l$inds[i] = j
          st = j+1
          break
        }
      } 
    }
    
    # identify lower-order nodes that are not perfectly nested
    missing.inds = which(grid.l$inds==0)
    # map missing lower-order nodes to end of higher-order grid
    grid.l$inds[missing.inds] = nrow(grid$nodes) + 1:length(missing.inds)
    # add missing lower-order nodes
    grid$nodes = rbind(grid$nodes, grid.l$nodes[missing.inds,])
    # ensure lower-order nodes don't get used in higher-order integration
    grid$weights = rbind(grid$weights, 
                         matrix(rep(0, length(missing.inds), ncol=1))
                         )
  }
  
  # evaluate the weight function at each quadrature point
  grid$d = apply(dnorm(grid$nodes, log = TRUE), 1, sum)
  # center and scale integration grid around posterior mode
  grid$nodes = sweep(t(solve(prec.chol, t(grid$nodes))), 2, - mu)
  # add jacobian
  grid$d = grid$d + sum(log(diag(prec.chol)))
  
  if(quadError) {
    # re-order lower order grid by increasing index in higher order grid
    o = order(grid.l$inds)
    
    # save approximation error grid inside the main grid
    grid$errorNodes = list(
      inds = grid.l$inds[o],
      weights = grid.l$weights[o]
    )
  }

  grid
}