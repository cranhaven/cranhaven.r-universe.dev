#' Generate a functional moving average process.
#'
#' The purpose is to simulate a functional autoregressive process of the form
#' \deqn{
#' X_t(u)=\sum_{k=1}^p \int_0^1\Psi_k(u,v) X_{t-k}(v)dv+\varepsilon_t(u),\quad 1\leq t\leq n.
#' }
#' Here we assume that the observations lie in a finite dimensional subspace of the function space spanned by
#' Fourier basis functions \eqn{\boldsymbol{b}^\prime(u)=(b_1(u),\ldots, b_d(u))}. That is \eqn{X_t(u)=\boldsymbol{b}^\prime(u)\boldsymbol{X}_t}, \eqn{\varepsilon_t(u)=\boldsymbol{b}^\prime(u)\boldsymbol{\varepsilon}_t} and \eqn{\Psi_k(u,v)=\boldsymbol{b}^\prime(u)\boldsymbol{\Psi}_k \boldsymbol{b}(v)}. Then it follows that
#' \deqn{
#' \boldsymbol{X}_t=\boldsymbol{\Psi}_1\boldsymbol{X}_{t-1}+\cdots+ \boldsymbol{\Psi}_p\boldsymbol{X}_{t-p}+\boldsymbol{\varepsilon}_t.
#' }
#' Hence the dynamic of the functional time series is described by a VAR(\eqn{p}) process.
#' 
#' In this mathematical model the law of \eqn{\boldsymbol{\varepsilon}_t} is determined by \code{noise}. The matrices \code{Psi[,,k]}
#' correspond to \eqn{\boldsymbol{\Psi}_k}. If \code{op.norms} is provided, then the coefficient matrices will be rescaled, such that
#' the Hilbert-Schmidt norms of \eqn{\boldsymbol{\Psi}_k} correspond to the vector.
#' 
#' @title Simulate functional moving average processes
#' 
#' @param n number of observations to generate.
#' @param d dimension of the underlying multivariate VAR model.
#' @param Psi an array of \eqn{p\geq 1} coefficient matrices (need to be square matrices). \code{Psi[,,k]} is the k-th coefficient
#' matrix. If \code{Psi} is provided then \code{d=dim(Psi)[1]}. If no value is set then we generate a functional autoregressive
#' process of order 1. Then, \code{Psi[,,1]} is proportional to \eqn{\exp(-|i-j|\colon 1\leq i, j\leq d)} and such that
#' Hilbert-Schmidt norm of the corresponding lag-1 MA operator is 1/2.
#' @param op.norms a vector with non-negative scalar entries to which the \code{Psi} are supposed to be scaled.
#' The length of the vector must equal to the order of the model.
#' @param noise \code{"mnorm"} for normal noise or \code{"mt"} for student t noise. If
#' not specified \code{"mvnorm"} is chosen.
#' @param sigma covariance  or scale matrix of the coefficients corresponding to functional innovations. The default value
#' is \code{diag(d:1)/d}.
#' @param df degrees of freqdom if \code{noise = "mt"}.
#' @return An object of class \code{\link[fda]{fd}}.
#' @seealso The multivariate equivalent in the \code{freqdom} package: \code{\link[freqdom]{rma}}
#' @keywords simulations
#' @export
#' @examples 
#' # Generate a FMA process without burnin (starting from 0)
#' fts = fts.rma(n = 5, d = 5)
#' plot(fts)
#' 
#' # Generate observations with very strong dependance
#' fts = fts.rma(n = 100, d = 5, op.norms = 0.999)
#' plot(fts)
#' 
#' # Generate observations with very strong dependance and noise
#' # from the multivariate t distribution
#' fts = fts.rma(n = 100, d = 5, op.norms = 0.999, noise = "mt")
#' plot(fts)
fts.rma = function(n=100, 
d=11, Psi = NULL, op.norms = NULL, noise="mnorm", sigma=diag(d:1)/d, df=4)
{
  
  if(!is.null(Psi) && d!=dim(Psi)[1])
  	stop("d must be equal to the dimension of Psi")

  if(!is.null(Psi) && dim(Psi)[1]!=dim(Psi)[2])
	  stop("coefficients need to be square matrices")

  if(!is.null(Psi))
  	d=dim(Psi)[1]

  if(d %% 2==0)
	  stop("d must be odd")
  
  if (is.null(Psi)){
		Psi = exp(-(1:d))%*%t(exp(-(1:d)))
		Psi = Psi/norm(Psi,type='F')/2
	}  	

  if(is.matrix(Psi)){
  	w=array(0,c(dim(Psi)[1],dim(Psi)[2],1))	
  	w[,,1]=Psi
  	Psi=w
  }	
  
  p=dim(Psi)[3]

  if(is.null(op.norms)){
    op.norms=c()
    for(i in 1:p){op.norms=c(op.norms,norm(Psi[,,i],type="f"))}
  }

  for(i in 1:p){
 	  Psi[,,i]=Psi[,,i]/norm(Psi[,,i],type="f")*op.norms[i]
  }	
  
  arg <- list()
  arg[['n']] <- n
  arg[['Psi']] <- Psi  
  arg[['noise']] <- noise
  arg[['sigma']] <- sigma	
  arg[['df']] <- df
  X=do.call(rma, arg)

  basis=create.fourier.basis(nbasis=d)
  fd(t(X),basis)
}

