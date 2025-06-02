#' Hyperparameter tuning for the ENNreg model using cross-validation
#'
#' \code{ENNreg_cv} tunes parameters xi and rho of the ENNreg model using cross-validation.
#'
#' Either the folds (a vector of the same length as y, such that \code{folds[i]} equals the
#' fold, between 1 and Kfold, containing observation i), or the number of folds must be provided.
#' Arguments \code{options} and \code{opt.rmsprop} are passed to function \code{\link{ENNreg}}.
#'
#' @param X Input matrix of size n x p, where n is the number of objects and p the number of
#' attributes.
#' @param y Vector of length n containing observations of the response variable.
#' @param K Number of prototypes.
#' @param batch If TRUE (default), batch learning is used; otherwise, online learning is
#' used.
#' @param folds Vector of length n containing the folds (integers between 1 and Kfold).
#' @param Kfold Number of folds (default=5, used only if \code{folds} is not provided).
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
#' @seealso \code{\link{ENNreg}}, \code{\link{ENNreg_holdout}}
#' @examples # Boston dataset
#' \donttest{
#' library(MASS)
#' X<-as.matrix(scale(Boston[,1:13]))
#' y<-Boston[,14]
#' set.seed(220322)
#' n<-nrow(Boston)
#' ntrain<-round(0.7*n)
#' train <-sample(n,ntrain)
#' cv<-ENNreg_cv(X=X[train,],y=y[train],K=30,XI=c(0.1,1,10),RHO=c(0.1,1,10))
#' cv$RMS
#' fit <- ENNreg(X[train,],y[train],K=30,xi=cv$xi,rho=cv$rho)
#' pred<-predict(fit,newdata=X[-train,],yt=y[-train])
#' print(pred$RMS)
#' }
ENNreg_cv<-function(X,y,K,batch=TRUE,folds=NULL,Kfold=5,XI,RHO,nstart=100,c=1,
                    lambda=0.9, eps=NULL,nu=1e-16,optimProto=TRUE,verbose=TRUE,
                    options=list(maxiter=1000,rel.error=1e-4,print=10),
                    opt.rmsprop=list(batch_size=100,epsi=0.001,rho=0.9,delta=1e-8,Dtmax=100)){
  if(is.null(eps)) eps<-0.01*sd(y)
  n<-length(y)
  if(is.null(folds)){
    ii<-sample(n,n)
    folds<-rep(1,n)
    for(k in 1:Kfold) folds[ii[seq(k,n,Kfold)]]<-k
  }  else Kfold <- max(folds)
  N1<-length(XI)
  N2<-length(RHO)
  ERRcv<-matrix(0,N1,N2)
  X<-as.matrix(X)
  for(k in 1:Kfold){
    if(verbose) cat("Fold",k,"\n")
    init <-ENNreg_init(X[folds!=k,],y[folds!=k],K=K,nstart,c)
    for(i in 1:N1){
      for(j in 1:N2){
        fit<-ENNreg(X[folds!=k,],y[folds!=k],init=init,K=K,batch=batch,lambda=lambda,
                           xi=XI[i],rho=RHO[j],eps=eps,nu=nu,optimProto=optimProto,verbose=FALSE,
                           options=options,opt.rmsprop=opt.rmsprop)
        pred<-predict(fit,newdata=X[folds==k,])
        ERRcv[i,j]<-ERRcv[i,j]+sum((y[folds==k]-pred$mux)^2)
      }
    }
  }
  RMS<-sqrt(ERRcv/n)
  imin<-which.min(apply(RMS,1,min))
  jmin<-which.min(apply(RMS,2,min))
  if(verbose) cat("Best hyperparameter values:","\n","xi =",XI[imin], "rho =",RHO[jmin],"\n")
  return(list(xi=XI[imin],rho=RHO[jmin],RMS=RMS))
}

