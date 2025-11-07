#' From a given spectral density operator the dynamic principal component filter sequences are computed.
#' 
#' Dynamic principal components are linear filters \eqn{(\phi_{\ell k}(u)\colon k\in \mathbf{Z})}, \eqn{1\leq\ell\leq d}.
#' They are defined as the Fourier coefficients of the dynamic eigenvector \eqn{\varphi_\ell(\omega)(u)} of a spectral density
#' kernel \eqn{f_\omega(u,v)}, i.e. \eqn{\int_0^1 f_\omega(u,v)\varphi_\ell(\omega)(v)dv=\lambda_\ell(\omega)\varphi_\ell(\omega)(u)}
#' and
#' \deqn{
#'   \phi_{\ell k}(u):=\frac{1}{2\pi}\int_{-\pi}^\pi \varphi_\ell(\omega)(u) \exp(-ik\omega) d\omega.
#' }
#' The index \eqn{\ell} is referring to the \eqn{\ell}-th largest dynamic eigenvalue \eqn{\lambda_\ell(\omega)}. For a given spectral
#' density operator (provided as on object of class \code{fts.freqdom}) the function \code{fts.dpca.filters} computes
#' \eqn{\phi_{\ell k}(u)} for \eqn{|k|\leq} q. Filters will be computed for \eqn{1\leq \ell\leq \code{Ndpc}}.
#' 
#' For more details we refer to  Hormann et al. (2015).
#' 
#' @title Functional dynamic PCA filters
#' 
#' @param F spectral density operator, provided as an object of class \code{fts.freqdom}.
#' @param Ndpc an integer \eqn{\in\{1,\ldots, d\}} with \eqn{d=}\code{F$basisX$nbasis}. It is the number of dynamic principal components to be computed. By default it is set equal to \eqn{d}.
#' @param q a non-negative integer. DPCA filter coefficients at lags \eqn{|h|\leq} q will be computed. By default \code{q=30}.
#' @return An object of class \code{fts.timedom}.  The list has the following components:
#' * \code{operators} \eqn{\quad} an array. Each matrix in this array has dimension \eqn{\code{Ndpc}\times d} and is assigned to a certain lag.
#' For a given lag \eqn{k}, the rows of the matrix correspond to the coefficient vector of the filter functions.
#' * \code{lags} \eqn{\quad} a vector with the lags of the filter coefficients.
#' * \code{basisX} \eqn{\quad} \code{F$basis}, hence an object of class \code{basis.fd} (see \code{\link[fda]{create.basis}}).
#' * \code{correspondence} \eqn{\quad} the correspondence matrix: all scalar products between basis functions.
#' @references Hormann, S., Kidzinski, L., and Hallin, M.
#' \emph{Dynamic functional principal components.} Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology) 77.2 (2015): 319-348.
#' @references Brillinger, D.
#' \emph{Time Series} (2001), SIAM, San Francisco.
#' @references Shumway, R.H., and Stoffer, D.S.
#' \emph{Time Series Analysis and Its Applications} (2006), Springer, New York.
#' @seealso The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{dpca.filters}}
#' @export
#' @keywords DPCA
#' @examples 
#' data(pm10)
#' X = center.fd(pm10)
#' 
#' # Compute the spectral density operator with Bartlett weights
#' SD = fts.spectral.density(X, freq = (-50:50/50) * pi, q = 2, weight="Bartlett")
#' filters = fts.dpca.filters(SD, 2, q = 10)
#' 
#' # Plot filters 1 and 2
#' fts.plot.filters(filters, 2, one.plot = TRUE)
#' 
#' # Recompute with a different estimate of the spectral density (largerg q)
#' SD = fts.spectral.density(X, freq = (-50:50/50) * pi, q = 5, weight="Bartlett")
#' filters = fts.dpca.filters(SD, 2, q = 10)
#' 
#' # Plot filters 1 and 2
#' fts.plot.filters(filters, 2, one.plot = TRUE)
fts.dpca.filters = function(F, Ndpc = F$basisX$nbasis, q = 30){
  if(!is.fts.freqdom(F))
  stop("F must be of class fts.freqdom")	

  if(dim(F$operators)[1]!= dim(F$operators)[2])
  stop("coefficients must be square matrices")

  B=inprod(F$basisX,F$basisX)
  B.root=eigen(B)$vectors%*%diag(sqrt(eigen(B)$values))%*%  t(eigen(B)$vectors)
  B.root.minus=solve(B.root)	
  
  n=dim(F$operators)[3]
    
  for(i in 1:n){
    F$operators[,,i]=B.root%*%F$operators[,,i]%*%B.root
  }
  
  multF=freqdom(F$operators,F$freq)  
  
  class(F) = "freqdom"
  A = dpca.filters(F=F, Ndpc = Ndpc, q = q)
  
  nfilters=dim(A$operators)[3]
  ncomp=dim(A$operators)[1]
  
  if(ncomp==1){
    for(i in 1:nfilters){
    	A$operators[,,i]=t(B.root.minus%*%as.matrix(A$operators[,,i]))
    }
  }
  else{
    for(i in 1:nfilters){
    	A$operators[,,i]=t(B.root.minus%*%t(A$operators[,,i]))
    }
  }
  fts.timedom(A, F$basisX, F$basisX)
}
