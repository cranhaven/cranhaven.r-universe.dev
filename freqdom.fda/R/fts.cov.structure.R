#' This function is used to estimate a collection of cross-covariances operators of two stationary functional series.
#' 
#' Let \eqn{X_1(u),\ldots, X_T(u)} and \eqn{Y_1(u),\ldots, Y_T(u)} be two samples of functional data. This function determines empirical lagged covariances between the series \eqn{(X_t(u))} and \eqn{(Y_t(u))}. More precisely it determines
#' \deqn{
#'   (\widehat{c}^{XY}_h(u,v)\colon h\in lags ),
#' }
#' where \eqn{\widehat{c}^{XY}_h(u,v)} is the empirical version of the covariance kernel \eqn{\mathrm{Cov}(X_h(u),Y_0(v))}.
#' For a sample of size \eqn{T} we set \eqn{\hat\mu^X(u)=\frac{1}{T}\sum_{t=1}^T X_t(u)} and
#' \eqn{\hat\mu^Y(v)=\frac{1}{T}\sum_{t=1}^T Y_t(v)}. Now for \eqn{h \geq 0}
#' \deqn{\frac{1}{T}\sum_{t=1}^{T-h} (X_{t+h}(u)-\hat\mu^X(u))(Y_t(v)-\hat\mu^Y(v))}
#' and for \eqn{h < 0}
#' \deqn{\frac{1}{T}\sum_{t=|h|+1}^{T} (X_{t+h}(u)-\hat\mu^X(u))(Y_t(v)-\hat\mu^Y(v)).}
#' Since \eqn{X_t(u)=\boldsymbol{b}_1^\prime(u)\mathbf{x}_t} and \eqn{Y_t(u)=\mathbf{y}_t^\prime \boldsymbol{b}_2(u)} we can write 
#' \deqn{
#'   \widehat{c}^{XY}_h(u,v)=\boldsymbol{b}_1^\prime(u)\widehat{C}^{\mathbf{xy}}\boldsymbol{b}_2(v),
#' }
#' where \eqn{\widehat{C}^{\mathbf{xy}}} is defined as for the function ``cov.structure'' for series of coefficient vectors 
#' \eqn{(\mathbf{x}_t\colon 1\leq t\leq T)} and \eqn{(\mathbf{y}_t\colon 1\leq t\leq T)}.
#' 
#' @title Estimate autocovariance and cross-covariances operators
#' 
#' @param X an object of class \code{\link[fda]{fd}} containing \eqn{T} functional observations.
#' @param Y an object of class \code{\link[fda]{fd}} containing \eqn{T} functional observations.
#' @param lags an integer-valued vector \eqn{(\ell_1,\ldots, \ell_K)} containing the lags for which covariances are calculated.
#' @return An object of class \code{\link{fts.timedom}}. The list contains the following components:
#' * \code{operators} \eqn{\quad} an array. Element \code{[,,k]} contains the covariance matrix of the coefficient vectors of the two time series related to lag \eqn{\ell_k}.
#' * \code{lags} \eqn{\quad} the lags vector from the arguments.
#' * \code{basisX} \eqn{\quad} \code{X$basis}, an object of class \code{basis.fd} (see \code{\link[fda]{create.basis}})
#' * \code{basisY} \eqn{\quad} \code{Y$basis}, an object of class \code{basis.fd} (see \code{\link[fda]{create.basis}})
#' @seealso The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{cov.structure}}
#' @export
#' @keywords time-domain
#' @examples 
#' # Generate an autoregressive process
#' fts = fts.rar(d=3)
#' 
#' # Get covariance at lag 0
#' fts.cov.structure(fts, lags = 0)
#' 
#' # Get covariance at lag 10
#' fts.cov.structure(fts, lags = 10)
#'
#' # Get entire covariance structure between -20 and 20
#' fts.cov.structure(fts, lags = -20:20)
#' 
#' # Compute covariance with another process
#' fts0 = fts + fts.rma(d=3)
#' fts.cov.structure(fts, fts0, lags = -2:2)
fts.cov.structure = function(X, Y=X, lags=0){
  fdom = cov.structure(X=t(X$coefs),Y=t(Y$coefs),lags=lags)
  fts.timedom(fdom, basisX=X$basis, basisY=Y$basis)
}
