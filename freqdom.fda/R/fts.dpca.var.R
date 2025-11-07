#' Computes the proportion and cumulative proportion of variance explained by dynamic principal components.
#' 
#' Consider a spectral density operator \eqn{\mathcal{F}_\omega} and let \eqn{\lambda_\ell(\omega)} by the \eqn{\ell}-th dynamic
#' eigenvalue. The proportion of variance described by the \eqn{\ell}-th dynamic principal component is given as
#' \eqn{v_\ell:=\int_{-\pi}^\pi \lambda_\ell(\omega)d\omega/\int_{-\pi}^\pi \mathrm{tr}(\mathcal{F}_\omega)d\omega}.
#' This function numerically computes the vectors \eqn{(v_\ell)}.
#' 
#' For more details we refer to Hormann et al. (2015).
#' 
#' @title Proportion of variance explained by dynamic principal components
#' 
#' @param F spectral density operator, provided as an object of class \code{\link{fts.freqdom}}. To guarantee accuracy of
#' numerical integration it is important that \code{F$freq} is a dense grid of frequencies in \eqn{[-\pi,\pi]}.
#' @return A vector containing the \eqn{v_\ell}.
#' @references Hormann, S., Kidzinski, L., and Hallin, M.
#' \emph{Dynamic functional principal components.} Journal of the Royal
#' Statistical Society: Series B (Statistical Methodology) 77.2 (2015): 319-348.
#' @seealso The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{dpca.var}}
#' @export
#' @keywords DPCA
fts.dpca.var = function(F){
  if (!is.fts.freqdom(F))
    stop("F must be an object of class fts.freqdom")

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
  
  dpca.var(multF)


    
}
