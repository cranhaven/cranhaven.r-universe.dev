#' Hyperparameter tuning for the ENNreg model using the hold-out method
#'
#' \code{ENNreg_holdout tunes parameters xi and rho of the ENNreg model using the
#' hold-out method.}
#'
#' Either the validation set (a vector of indices), or the number nval of validation
#' instances must be provided. Arguments \code{options} and \code{opt.rmsprop} are passed
#' to function \code{\link{ENNreg}}.
#'
#' @param X Input matrix of size n x p, where n is the number of objects and p the number of
#' attributes.
#' @param y Vector of length n containing observations of the response variable.
#' @param K Number of prototypes.
#' @param batch If TRUE (default), batch learning is used; otherwise, online learning is
#' used.
#' @param val Vector of indices of the validation instances (nval integers between 1 and n).
#' Needed only if \code{nval} is not provided.
#' @param nval Number of validation instances (needed only if \code{val} is not provided).
#' @param XI Vector of candidate values for hyperparameter \code{xi}.
#' @param RHO Vector of candidate values for hyperparameter \code{rho}.
#' @param nstart Number of random starts of the k-means algorithm (default: 100).
#' @param c Multiplicative coefficient applied to scale parameter gamma (defaut: 1).
#' @param lambda Parameter of the loss function (default=0.9).
#' @param eps Parameter of the loss function (if NULL, fixed to 0.01 times the standard
#' deviation of y).
#' @param nu Parameter of the loss function to avoid a division par zero (default=1e-16).
#' @param optimProto If TRUE (default), the initial prototypes are optimized.
#' @param verbose If TRUE (default) intermediate results are displayed.
#' @param options Parameters of the optimization algorithm (see \code{\link{ENNreg}}).
#' @param opt.rmsprop Parameters of the RMSprop algorithm (see \code{\link{ENNreg}}).
#'
#' @return A list with three components:
#' \describe{
#' \item{xi}{Optimal value of xi.}
#' \item{rho}{Optimal value of rho.}
#' \item{RMS}{Matrix of root mean squared error values}.
#' }
#' @export
#' @importFrom stats sd predict
#'
#' @references
#'
#' Thierry Denoeux. An evidential neural network model for regression based on random fuzzy
#' numbers. In "Belief functions: Theory and applications (proc. of BELIEF 2022)", pages 57-66,
#' Springer, 2022.
#'
#' Thierry Denoeux. Quantifying prediction uncertainty in regression using random fuzzy sets: the ENNreg
#' model. IEEE Transactions on Fuzzy Systems, Vol. 31, Issue 10, pages 3690-3699, 2023.
#'
#' @seealso \code{\link{ENNreg}}, \code{\link{ENNreg_cv}}
#' @examples # Boston dataset
#' \donttest{
#' library(MASS)
#' X<-as.matrix(scale(Boston[,1:13]))
#' y<-Boston[,14]
#' set.seed(220322)
#' n<-nrow(Boston)
#' hold.out<-ENNreg_holdout(X,y,K=30,nval=round(n/3),XI=c(0.1,1,10),RHO=c(0.1,1,10))
#' hold.out$RMS
#' }
#'
ENNreg_holdout<-function(X,y,K,batch=TRUE,val=NULL,nval=NULL,XI,RHO,nstart=100,c=1,
                         lambda=0.9, eps=NULL,nu=1e-16,optimProto=TRUE,verbose=TRUE,
                         options=list(maxiter=1000,rel.error=1e-4,print=10),
                         opt.rmsprop=list(batch_size=100,epsi=0.001,rho=0.9,delta=1e-8,
                                          Dtmax=100)){
  if(is.null(eps)) eps<-0.01*sd(y)
  n<-length(y)
  if(is.null(val)){
    if(is.null(nval))
      stop("Validation set or number of validation instances must be supplied") else
        val<-sample(n,nval,replace=FALSE)
  } else nval<-length(val)
  N1<-length(XI)
  N2<-length(RHO)
  RMS<-matrix(0,N1,N2)
  X<-as.matrix(X)
  init <-ENNreg_init(X[-val,],y[-val],K=K,nstart,c)
  for(i in 1:N1){
    for(j in 1:N2){
      fit<-ENNreg(X[-val,],y[-val],init=init,K=K,batch=batch,lambda=lambda,
                  xi=XI[i],rho=RHO[j],eps=eps,nu=nu,optimProto=optimProto,verbose=FALSE,
                  options=options,opt.rmsprop=opt.rmsprop)
      pred<-predict(fit,newdata=X[val,],y[val])
      RMS[i,j]<-pred$RMS
      if(verbose) cat("xi =",XI[i], "rho =",RHO[j],"rms =",RMS[i,j],"\n")
    }
  }

  imin<-which.min(apply(RMS,1,min))
  jmin<-which.min(apply(RMS,2,min))
  if(verbose) cat("Best hyperparameter values:","\n","xi =",XI[imin], "rho =",RHO[jmin])
  return(list(xi=XI[imin],rho=RHO[jmin],RMS=RMS))
}

