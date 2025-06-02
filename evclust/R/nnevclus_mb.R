#' NN-EVCLUS algorithm (minibatch version)
#'
#'\code{nnevclus_mb} computes a credal partition from a dissimilarity matrix using the 
#'NN-EVCLUS algorithm. Training is done using mini-batch gradient descent with the RMSprop
#'optimization algorithm.
#'
#' This is a neural network version of \code{kevclus}. The neural net has one layer
#' of ReLU units and a softmax output layer (see Denoeux, 2020). The network is trained
#' in mini-batch mode using the RMSprop algorithm. The inputs are a feature vector x, 
#' an optional distance matrix D, and an optional vector of one-class SVM outputs fhat, 
#' which is used for novelty detection. Part of the output belief mass is transfered to 
#' the empty set based on beta[1]+beta[2]*fhat, where beta is an additional parameter 
#' vector. The network can be trained in fully unsupervised mode or in semi-supervised mode
#' (with class labels for a subset of the learning instances). The output is a credal 
#' partition (a "credpart" object), with a specific field containing the network parameters (U, V, W,
#' beta).
#'
#' @param x nxp matrix of p attributes observed for n objects.
#' @param foncD A function to compute distances.
#' @param c Number of clusters
#' @param type Type of focal sets ("simple": empty set, singletons and Omega;
#' "full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
#' \eqn{\Omega}, and all or selected pairs).
#' @param n_H Size or the hidden layer.
#' @param nbatch Number of mini-batches.
#' @param alpha0 Order of the quantile to normalize distances. Parameter d0 is set to 
#' the alpha0-quantile of distances. Default: 0.9.
#' @param fhat Vector of outputs from a one-class SVM for novelty detection (optional)
#' @param lambda Regularization coefficient (default: 0)
#' @param y Optional vector of class labels for a subset of the training set 
#' (for semi-supervised learning).
#' @param Is Vector of indices corresponding to y (for semi-supervised learning).
#' @param nu Coefficient of the supervised error term (default: 0).
#' @param disp If TRUE, intermediate results are displayed.
#' @param options Parameters of the optimization algorithm (Niter: maximum number of
#' iterations; epsi, rho, delta: parameters of RMSprop; Dtmax: the algorithm stops when
#' the loss has not decreased in the last Dtmax iterations; print: number of iterations 
#' between two displays).
#' @param param0 Optional list of initial network parameters (see details).
#' 
#' #' 
#' @return The output credal partition (an object of class \code{"credpart"}). In 
#' addition to the usual attributes, the output credal partition has the following 
#' attributes:
#'  \describe{
#'   \item{Kmat}{The matrix of degrees of conflict. Same size as D.}
#'   \item{trace}{Trace of the algorithm (values of the loss function).}
#'   \item{param}{The network parameters as a list with components V, W and beta.}
#'  }
#'
#'
#'@references T. Denoeux. NN-EVCLUS: Neural Network-based Evidential Clustering. 
#'Information Sciences, Vol. 572, Pages 297-330, 2021.
#'
#'@author Thierry Denoeux.
#'
#' @export
#' @importFrom stats runif quantile
#'
#' @seealso \code{\link{nnevclus}}, \code{\link{predict.credpart}}, 
#' \code{\link{kevclus}}, \code{\link{kcevclus}}, \code{\link{harris}}
#'
#' @examples 
#'\dontrun{
#'## Unsupervised learning
#' data(fourclass)
#' x<-scale(fourclass[,1:2])
#' y<-fourclass[,3]
#' svmfit<-ksvm(~.,data=x,type="one-svc",kernel="rbfdot",nu=0.2,kpar=list(sigma=0.2))
#' fhat<-predict(svmfit,newdata=x,type="decision")
#' clus<-nnevclus_mb(x,foncD=function(x) as.matrix(dist(x)),c=4,type='pairs',
#' n_H=10,nbatch=10,alpha0=0.9,fhat=fhat)
#' plot(clus,x)
#' ## semi-supervised learning
#' Is<-sample(400,100)
#' clus<-nnevclus_mb(x,foncD=function(x) as.matrix(dist(x)),c=4,type='pairs',
#' n_H=10,nbatch=10,alpha0=0.9,fhat=fhat,lambda=0, y=y[Is],Is=Is,nu=0.5)
#' plot(clus,x)
#' }
#'
nnevclus_mb<-function(x,foncD=function(x) as.matrix(dist(x)),c,type='simple',n_H,
                         nbatch=10,alpha0=0.9,fhat=NULL,lambda=0, 
                         y=NULL,Is=NULL,nu=0,
                         disp=TRUE,
                         options=list(Niter=1000,epsi=0.001,rho=0.9,
                                      delta=1e-8,Dtmax=100,print=5),
                         param0=list(V0=NULL,W0=NULL,beta0=NULL)){
  
  # Version with distances computed by function foncD
  V0<-param0$V0
  W0<-param0$W0
  beta0<-param0$beta0
  x<-as.matrix(x)
  n<-nrow(x)
  d<-ncol(x)+1
  X<-as.matrix(cbind(rep(1,n),x))
  
  # distance normalization
  N<-min(n,2000)
  ii<-sample(n,N)
  d0<-quantile(foncD(x[ii,]),alpha0)
  gam=-log(0.05)/d0^2
  
  F<-makeF(c=c,type=type,pairs=NULL)
  f<-nrow(F)
  matrices<-build_matrices(F)
  xi<-matrices$C
  
  N_V<-n_H*d
  N_W<-f*(n_H+1)
  
  if(is.null(y)|(is.null(Is))) nu<-0
  if(nu>0){
    semi<-TRUE
    ns<-length(Is)
    if(ns/nbatch>=10) do_batch_s=TRUE else do_batch_s=FALSE
  } else semi<-FALSE
  
  # Initialization
  if(!is.null(V0) & !is.null(W0) & !is.null(beta0)) init<-TRUE else init<-FALSE
  if(init) theta<-c(as.vector(V0),as.vector(W0),beta0) else
  {
    V0<-matrix(runif(N_V,min=-1/sqrt(d),max=1/sqrt(d)),n_H,d)
    W0<-matrix(runif(N_W,min=-1/sqrt(n_H+1),max=1/sqrt(n_H+1)),f,n_H+1)
    theta<-c(as.vector(V0),as.vector(W0))
    if(is.null(fhat)) theta<-c(theta,c(0,0)) else theta<-c(theta,c(0,-1))
  }  
  
  N<-length(theta)
  go_on<-TRUE
  r<-rep(0,N)
  #  s<-rep(0,N)
  error.best<-Inf
  Error<-NULL
  t<-0
  while(go_on){  # MAIN LOOP
    t<-t+1
    batch=sample(1:nbatch,n,replace=TRUE)
    if(semi) batch_s=sample(1:nbatch,ns,replace=TRUE)
    error<-0
    for(k in 1:nbatch){ # Loop on mini-batches
      ii<-which(batch==k)
      Dii<-1-exp(-gam*foncD(x[ii,])^2)
      if(is.null(fhat)){
        fg<-foncgrad_online_part1(theta,X[ii,],Dii,fhat=NULL,xi)
      } else{
        fg<-foncgrad_online_part1(theta,X[ii,],Dii,fhat=fhat[ii],xi)
      } #endif
      g<-(1-nu)*fg$grad + 
        lambda*c(theta[1:N_V]/N_V,theta[(N_V+1):(N_V+N_W)]/N_W,c(0,0))
      error<-error+(1-nu)*fg$fun + 0.5*lambda*(mean(theta[1:N_V]^2)+
                                                 mean(theta[(N_V+1):(N_V+N_W)]^2))
      if(semi){
        if(do_batch_s) iis<-which(batch_s==k) else iis=1:ns
        if(is.null(fhat)){
          fg2<-foncgrad_online_part2(theta,X[Is[iis],],fhat=NULL,F,y[iis])
        } else{
          fg2<-foncgrad_online_part2(theta,X[Is[iis],],fhat[Is[iis]],F,y[iis])
        }
        g<-g+nu*fg2$grad
        error<-error+nu*fg2$fun
      } # endif semi
      #    RMSprop
      r<-options$rho*r+(1-options$rho)*g*g
      theta<-theta -(options$epsi/(options$delta+sqrt(r)))*g
    } #endfor k
    error<- error/nbatch  
    if(error<error.best){
      theta.best<-theta
      t.best<-t
      error.best<-error
    }
    if((t>options$Niter) | (t-t.best > options$Dtmax)) go_on<-FALSE
    Error<-c(Error,error)
    if(disp & (t-1)%%options$print==0) print(c(t,error))
  } # end main loop
  
  theta<-theta.best
  V<-matrix(theta[1:N_V],n_H,d)
  W<-matrix(theta[(N_V+1):(N_V + N_W)],f,n_H+1)
  beta<-theta[N_V + N_W+c(1,2)] 
  Zeros<-matrix(0,n,n_H)
  # Propagation
  A<-X%*%t(V)
  Z<-cbind(rep(1,n),pmax(Zeros,A))
  alpha<-Z%*%t(W)
#  mass<-exp(alpha)
  mass<-exp(alpha-apply(alpha,1,max))
  mass<-mass/rowSums(mass)
  if(is.null(fhat)) gam<-rep(0,n) else{
    eta<-log(1+exp(beta[1]+beta[2]*fhat))
    gam<-eta/(1+eta)
    mass<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
  }
  K<- mass %*% xi %*% t(mass)
  trace<-Error
  
  clus<-extractMass(mass=mass,F=F,method="nn-evclus-minibatch",crit=error.best,
                    trace=trace,Kmat=K,param=list(V=V,W=W,beta=beta))
  return(clus)
}
