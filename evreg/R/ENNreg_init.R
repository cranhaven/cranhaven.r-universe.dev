#' Parameter initialization for the ENNreg model
#'
#'\code{ENNreg_init} returns initial parameter values for the ENNreg model.
#'
#' Prototypes are initialized by the k-means algorithm.
#'
#' @param X Input matrix of size n x p, where n is the number of objects and p the number of
#' attributes.
#' @param y Vector of length n containing observations of the response variable.
#' @param K Number of prototypes.
#' @param nstart Number of random starts of the k-means algorithm (default: 100)
#' @param c Multiplicative coefficient applied to scale parameter gamma (defaut: 1)
#'
#' @return An object of class "ENNreg", which can be passed to function \code{\link{ENNreg}}.
#'
#' @author Thierry Denoeux.
#'
#' @export
#' @importFrom stats kmeans sd
#'
#' @seealso \code{\link{ENNreg}}
#'
#' @examples ## Boston dataset
#' library(MASS)
#' attach(Boston)
#' X <- as.matrix(scale(Boston[,1:13]))
#' y <- Boston[,14]
#' psi <- ENNreg_init(X,y,K=30)
ENNreg_init<-function(X,y,K,nstart=100,c=1){

  X<-as.matrix(X)
  p<-ncol(X)
  clus<-kmeans(X,centers=K,iter.max = 100,nstart=nstart)
  Beta<-matrix(0,K,p)
  alpha<-rep(0,K)
  sig<-rep(1,K)
  W<-clus$centers
  gam<-rep(1,K)
  for(k in 1:K){
    ii<-which(clus$cluster==k)
    nk<-length(ii)
    alpha[k]<-mean(y[ii])
    if(nk>1){
      gam[k]<-1/sqrt(1e-3+clus$withinss[k]/nk)
      sig[k]<-sd(y[ii])
    }
  }
  gam<-c*gam
  eta<-rep(2,K)
  h<-eta^2
  psi<-c(alpha,as.vector(Beta),sig,eta,gam,as.vector(W))
  init<-list(loss=NULL,param=list(alpha=alpha,Beta=Beta,sig=sig,h=h,gam=gam,W=W,psi=psi),
            K=K, pred=NULL, Einf=NULL, Esup=NULL)
  class(init)<-"ENNreg"
  return(init)
}
