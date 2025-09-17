#' Evaluate a mixture density
#' 
#' Evaluates mixture densities of the form
#' \deqn{f(x) = \sum_{j=1}^k f(x|\theta^{(k)}) w_k}
#' where the \eqn{w_k} are (possibly negative) weights that sum to 1 and 
#' \eqn{f(x|\theta^{(k)})} are densities that are specified via parameters
#' \eqn{\theta^{(k)}}, which are passed in the function argument 
#' \code{params}.
#' A unique feature of this function is that it is able to evaluate mixture
#' densities in which some of the mixture weights \eqn{w_k} are negative.
#' 
#' @export
#' 
#' @param x Points at which the mixture should be evaluated.  If the density 
#'   is multivariate, then each row of \code{x} should contain one set of 
#'   points at which the mixture should be evaluated.
#' @param f Density used in the mixture. The function should be defined so it 
#'   is can be called via \code{f(x, params, log, ...)}.  The density \eqn{f}
#'   is evaluated at the points in \code{x} using one set of parameters 
#'   \code{params}, i.e., for some specific \eqn{\theta^{(k)}}.
#'   if \code{log==TRUE}, then \eqn{ln(f)} is returned.  Additional parameters
#'   may be passed to \eqn{f} via \code{...}.
#' @param params Matrix in which each row contains parameters that define
#'   \eqn{f}.  The number of rows in \code{params} should match the number of 
#'   mixture components \eqn{k}.
#' @param wts vector of weights for each mixture component
#' @param log TRUE to return the log of the mixture density
#' @param errorNodesWts list with elements \code{inds} and \code{weights} that 
#'   point out which \code{params} get used to compute an approximation of the 
#'   quadrature error.
#' @param ... additional arguments to be passed to \code{f}
#' 
#' @example examples/dmixEx.R
#' 
dmix = function(x, f, params, wts, log = FALSE, errorNodesWts = NULL, ...){
  
  if(!is.matrix(x)) {
    x = matrix(x, ncol=1)
  }
  
  if(!is.matrix(params)) {
    params = matrix(params, ncol=1)
  }
  
  res = numeric(nrow(x))
  if(!is.null(errorNodesWts)) { res.l = numeric(nrow(x)) }
  
  for(i in 1:length(res)) {
    # evaluate mixture components
    lnf = apply(params, 1, function(params){
      f(x[i,], as.numeric(params), log = TRUE, ...)})
    
    # numerically stable evaluation of mixture density on log scale
    lnk = -mean(lnf)
    res[i] = log(sum(exp(lnf + lnk) * wts)) - lnk
    
    if(!is.null(errorNodesWts)) {
      lnf = lnf[errorNodesWts$inds]
      lnk = -mean(lnf)
      res.l[i] = log(sum(exp(lnf + lnk) * errorNodesWts$weights)) - lnk
    }
  }
  
  if(!is.null(errorNodesWts)) { 
    if(!log) { 
      res = exp(res)
      res.l = exp(res.l)
    }
    
    list(d = res, 
         d.coarse = res.l, 
         rel.err.bound = (res.l - res)/res * 100)
  } else {
    if(log) { res } else { exp(res) }
  }
}