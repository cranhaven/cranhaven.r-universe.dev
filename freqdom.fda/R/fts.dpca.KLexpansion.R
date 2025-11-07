#' Computes the dynamic KL expansion up to a given order.
#' 
#' This function computes the \eqn{L}-order dynamic functional principal components expansion, defined by
#' \deqn{
#'   \hat{X}_{t}^L(u):=\sum_{\ell=1}^L\sum_{k\in\mathbf{Z}}  Y_{\ell,t+k} \phi_{\ell k}(u),\quad 1\leq L\leq d,
#' }
#' where \eqn{\phi_{\ell k}(v)} and \eqn{d} are explained in  \code{fts.dpca.filters} and \eqn{Y_{\ell k}} are the dynamic functional PC scores as in \code{fts.dpca.scores}. For the sample version the sum extends over the range of lags for which the \eqn{\phi_{\ell k}} are defined. 
#' 
#' For more details we refer to Hormann et al. (2015).
#' 
#' @title Dynamic KL expansion
#' 
#' @param X a functional time series given as an object of class \code{\link[fda]{fd}}.
#' @param dpcs an object of class \code{fts.timedom}, representing the dpca filters
#' obtained from the sample \code{X}. If \code{dpsc = NULL}, then \code{dpcs = fts.dpca.filter(fts.spectral.density(X))} is used.
#' @return An object of class \code{\link[fda]{fd}}.
#' @references Hormann, S., Kidzinski, L., and Hallin, M.
#' \emph{Dynamic functional principal components.} Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology) 77.2 (2015): 319-348.
#' @seealso The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{dpca.KLexpansion}}
#' @export
#' @keywords DPCA
fts.dpca.KLexpansion = function(X, dpcs=fts.dpca.filters(fts.spectral.density(X))){
	Y=fts.dpca.scores(X,dpcs)
	dpcsmult=timedom(dpcs$operators,dpcs$lags)
	fd(t(Y %c% freqdom.transpose(rev(dpcsmult))),dpcs$basisY)
}
