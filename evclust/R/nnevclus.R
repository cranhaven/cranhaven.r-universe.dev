#' NN-EVCLUS algorithm
#'
#'\code{nnevclus} computes a credal partition from a dissimilarity matrix using the NN-EVCLUS
#'algorithm.
#'
#' This is a neural network version of \code{kevclus}. The neural net has one or two layers
#' of ReLU units and a softmax output layer (see Denoeux, 2020). The weight matrices are 
#' denoted by U, V and W for a two-hidden-layer network, or V and W for a one-hidden-layer 
#' network. The inputs are a feature vector x, an optional distance matrix D, and an 
#' optional vector of one-class SVM outputs fhat, which is used for novelty detection. 
#' Part of the output belief mass is transfered to the empty set based on beta[1]+beta[2]*fhat,
#' where beta is an additional parameter vector. The network can be trained in fully
#' unsupervised mode, in semi-supervised mode (with class labels for a subset of the
#' learning instances), or with pairwise constraints. The output is a credal partition 
#' (a "credpart" object), with a specific field containing the network parameters (U, V, W,
#' beta).
#'
#' @param x nxp matrix of p attributes observed for n objects.
#' @param k Number of distances to compute for each object (default: n-1).
#' @param D nxn or nxk dissimilarity matrix (optional). If absent, the Euclidean distance 
#' is computed.
#' @param J nxk matrix of indices. D[i,j] is the distance between objects i and
#' J[i,j]. (Used only if D is supplied and ncol(D)<n.)
#' @param c Number of clusters
#' @param type Type of focal sets ("simple": empty set, singletons and Omega;
#' "full": all \eqn{2^c} subsets of \eqn{\Omega}; "pairs": \eqn{\emptyset}, singletons,
#' \eqn{\Omega}, and all or selected pairs).
#' @param n_H Number of hidden units (if one hidden layer), or a two-dimensional vector
#' of numbers of hidden units (if two hidden layers).
#' @param ntrials Number of runs of the optimization algorithm (set to 1 if param0 is 
#' supplied).
#' @param d0 Parameter used for matrix normalization. The normalized distance corresponding
#' to d0 is 0.95.
#' @param fhat Vector of outputs from a one-class SVM for novelty detection (optional)
#' @param lambda Regularization coefficient (default: 0)
#' @param y Optional vector of class labels for a subset of the training set 
#' (for semi-supervised learning).
#' @param Is Vector of indices corresponding to y (for semi-supervised learning).
#' @param nu Coefficient of the supervised error term (default: 0).
#' @param ML Optional nbML*2 matrix of must-link constraints (for constrained clustering). 
#' Each row of ML contains the indices of objects that belong to the same class.
#' @param CL Optional nbCL*2 matrix of cannot-link constraints (for constrained clustering). 
#' Each row of CL contains the indices of objects that belong to different classes.
#' @param xi Coefficient of the constrained clustering loss (default: 0).
#' @param tr If TRUE, a trace of the stress function is returned.
#' @param options Parameters of the optimization algorithm (see \code{\link{harris}}).
#' @param param0 Optional list of initial network parameters (see details).
#' 
#' 
#' @return The output credal partition (an object of class \code{"credpart"}). In 
#' addition to the usual attributes, the output credal partition has the following 
#' attributes:
#'  \describe{
#'   \item{Kmat}{The matrix of degrees of conflict. Same size as D.}
#'   \item{D}{The normalized dissimilarity matrix.}
#'   \item{trace}{Trace of the algorithm (Stress function vs iterations).}
#'   \item{J}{The matrix of indices.}
#'   \item{param}{The network parameters as a list with components U, V, W and beta.}
#'  }
#'
#'@references T. Denoeux. NN-EVCLUS: Neural Network-based Evidential Clustering. 
#'Information Sciences, Vol. 572, Pages 297-330, 2021.
#'
#'@author Thierry Denoeux.
#'
#' @export
#' @importFrom stats runif quantile
#'
#' @seealso \code{\link{nnevclus_mb}}, \code{\link{predict.credpart}}, 
#' \code{\link{kevclus}}, \code{\link{kcevclus}}, \code{\link{harris}}
#'
#' @examples 
#'\dontrun{
#' ## Example with one hidden layer and no novelty detection
#' data(fourclass)
#' x<-scale(fourclass[,1:2])
#' y<-fourclass[,3]
#' clus<-nnevclus(x,c=4,n_H=c(5,5),type='pairs') # One hidden layer
#' plot(clus,x,mfrow=c(2,2))
#'
#' ## Example with two hidden layers and novelty detection
#' library(kernlab)
#' data(fourclass)
#' x<-scale(fourclass[,1:2])
#' y<-fourclass[,3]
#' x<-data.frame(x)
#' svmfit<-ksvm(~.,data=x,type="one-svc",kernel="rbfdot",nu=0.2,kpar=list(sigma=0.2))
#' fhat<-predict(svmfit,newdata=x,type="decision")
#' clus<-nnevclus(x,k=200,c=4,n_H=c(5,5),type='pairs',fhat=fhat)
#' plot(clus,x,mfrow=c(2,2))
#' 
#' ## Example with semi-supervised learning
#' data<-bananas(400)
#' x<-scale(data$x)
#' y<-data$y
#' Is<-sample(400, 50)  # Indices of labeled instances
#' plot(x,col=y,pch=y)
#' points(x[Is,],pch=16)
#' svmfit<-ksvm(~.,data=x,type="one-svc",kernel="rbfdot",nu=0.2,kpar=list(sigma=0.2))
#' fhat<-predict(svmfit,newdata=x,type="decision")
#' clus<-nnevclus(x,k=100,c=2,n_H=10,type='full',fhat=fhat,Is=Is,y=y[Is],nu=0.5)
#' plot(clus,x)
#' 
#' ## Example with pairwise constraints
#' data<-bananas(400)
#' x<-scale(data$x)
#' y<-data$y
#' const<-create_MLCL(y,500)
#' clus<-nnevclus(x,k=100,c=2,n_H=10,type='full',fhat=fhat,ML=const$ML,CL=const$CL,
#' rho=0.5)
#' plot(clus,x)
#' 
#' ## Example with pairwise constraints and PCCA
#' data(iris)
#' x<-scale(as.matrix(iris[,1:4]))
#' y<-as.integer(iris[,5])
#' const<-create_MLCL(y,100)
#' res.pcca<-pcca(x,3,const$ML,const$CL,beta=1)
#' plot(res.pcca$z,pch=y,col=y)
#' clus<-nnevclus(x=x,D=res.pcca$D,c=3,n_H=10,type='full',ML=const$ML,CL=const$CL,rho=0.5)
#' plot(clus,x[,3:4])
#' }
#'
nnevclus<-function(x,k=n-1,D=NULL,J=NULL,c,type='simple',n_H,ntrials=1,
                   d0=quantile(D,0.9), 
                   fhat=NULL,lambda=0,
                   y=NULL,Is=NULL,nu=0,
                   ML=NULL,CL=NULL,xi=0,
                   tr=FALSE,options=c(1,1000,1e-4,10),
                   param0=list(U0=NULL,V0=NULL,W0=NULL,beta0=NULL)){
  
  rho<-xi
  U0<-param0$U0
  V0<-param0$V0
  W0<-param0$W0
  beta0<-param0$beta0
  x<-as.matrix(x)
  n<-nrow(x)
  d<-ncol(x)+1
  X<-as.matrix(cbind(rep(1,n),x))
  if(is.null(y) | is.null(Is)) nu<-0
  if(is.null(ML) | is.null(CL)) rho<-0
  # 3 cases:
  # a- D is not supplied (J, if supplied, is ignored)
  # b- D is supplied, but not J: D and J are recomputed with k random columns
  # c- D and J are supplied (k is ignored)
  if(is.null(D)){ # Case 1: D is not supplied
    if(options[1]>0) print("D missing")
    res<-createD(x,k) # The Euclidean distance is assumed
    D<-res$D
    J<-res$J
    p<-k
  } else if(is.null(J)) { # Case 2: D is supplied, J not supplied
    if(options[1]>0) print("D is supplied, J missing")
    D<-as.matrix(D)
    if(n != ncol(D)){
      print('ERROR: D is not squared and J is not supplied!')
      return()
    }
    if(k == n-1){ 
      J<-matrix(0,n,n-1)
      D1<-J
      for(i in 1:n){
        J[i,]<-(1:n)[-i]
        D1[i,]<-D[i,J[i,]]
      } # end for
      D<-D1
      p<-n-1
    } else{ #  k<n
      D1<-matrix(0,n,k)
      J<-D1
      for(i in 1:n){
        ii<-sample((1:n)[-i],k)
        J[i,]<-ii
        D1[i,]<-D[i,ii]
      } # end for
      D<-D1
      p<-k
    } 
  } else {  # Case 3: D and J supplied
    if(options[1]>0) print("D and J supplied")
    p<-ncol(D)
    if((ncol(J)!=p)|(nrow(D)!=nrow(J))){
      print('ERROR: D and J must be of the same size!')
      return()
    }
  }
  
  # distance normalization
    g=-log(0.05)/d0^2
    D<-1-exp(-g*D^2)
 
  F<-makeF(c=c,type=type,pairs=NULL)
  f<-nrow(F)
  matrices<-build_matrices(F)
  Xi<-matrices$C
  Q<-Xi+1-matrices$E-matrices$S
  
  if(!is.null(V0) & !is.null(W0) & !is.null(beta0)){
    init<-TRUE
    ntrials<-1
  } else  init<-FALSE
  
  layer1<- (length(n_H)==1)
  
  Sbest=Inf
  
  if(layer1){
    N_V<-n_H*d
    N_W<-f*(n_H+1)
    for(N in 1:ntrials){
      if(!init){
        V0<-matrix(runif(N_V,min=-1/sqrt(d),max=1/sqrt(d)),n_H,d)
        W0<-matrix(runif(N_W,min=-1/sqrt(n_H+1),max=1/sqrt(n_H+1)),f,n_H+1)
        theta0<-c(as.vector(V0),as.vector(W0))
        if(is.null(fhat)) theta0<-c(theta0,c(0,0)) else theta0<-c(theta0,c(0,-1))
      } else theta0<-c(as.vector(V0),as.vector(W0),beta0)
      opt<-harris(foncgrad2_ReLU6,theta0,options=options,tr=tr,X=X,fhat=fhat,y=y,
                  Is=Is,D=D,J=J,Xi=Xi,F=F,lambda=lambda,nu=nu,ML=ML,
                  CL=CL,Q=Q,rho=rho)
      if(opt$value<Sbest){
        Sbest<-opt$value
        theta<-opt$par
        trbest<-opt$trace
      }
    } # endfor N
    U<-NULL
    V<-matrix(theta[1:N_V],n_H,d)
    W<-matrix(theta[(N_V+1):(N_V + N_W)],f,n_H+1)
    beta<-theta[N_V + N_W + c(1,2)] 
    
    Zeros<-matrix(0,n,n_H)
    # Propagation
    A<-X%*%t(V)
    Z<-cbind(rep(1,n),pmax(Zeros,A))
    alpha<-Z%*%t(W)
#    mass<-exp(alpha)
    mass<-exp(alpha-apply(alpha,1,max))
    mass<-mass/rowSums(mass)
    if(is.null(fhat)) gam<-rep(0,n) else{
      #   eta<-pmax(0,beta[1]+beta[2]*fhat)
      eta<-log(1+exp(beta[1]+beta[2]*fhat))
      gam<-eta/(1+eta)
      mass<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
    }
    K<-matrix(0,n,p)
    for(i in 1:n) K[i,]=mass[i,] %*% Xi %*% t(mass[J[i,],])
    
  } else{ # 2 hidden layers
    N_U<-n_H[1]*d
    N_V<-n_H[2]*(n_H[1]+1)
    N_W<-f*(n_H[2]+1)
    for(N in 1:ntrials){
      if(!init){
        U0<-matrix(runif(N_U,min=-1/sqrt(d),max=1/sqrt(d)),n_H[1],d)
        V0<-matrix(runif(N_V,min=-1/sqrt(n_H[1]+1),max=1/sqrt(n_H[1]+1)),n_H[2],n_H[1]+1)
        W0<-matrix(runif(N_W,min=-1/sqrt(n_H[2]+1),max=1/sqrt(n_H[2]+1)),f,n_H[2]+1)
        theta0<-c(as.vector(U0),as.vector(V0),as.vector(W0))
        if(is.null(fhat)) theta0<-c(theta0,c(0,0)) else theta0<-c(theta0,c(0,-1))
      } else theta0<-c(as.vector(U0),as.vector(V0),as.vector(W0),beta0)
      opt<-harris(foncgrad2_ReLU7,theta0,n_H,options=options,tr=tr,X=X,fhat=fhat,y=y,
                  Is=Is,D=D,J=J,Xi=Xi,F=F,lambda=lambda,nu=nu,ML=ML,
                  CL=CL,Q=Q,rho=rho)
      if(opt$value<Sbest){
        Sbest<-opt$value
        theta<-opt$par
        trbest<-opt$trace
      }
    } # endfor N
    N<-length(theta)
    U<-matrix(theta[1:N_U],n_H[1],d)
    V<-matrix(theta[(N_U+1):(N_U+N_V)],n_H[2],n_H[1]+1)
    W<-matrix(theta[(N_U+N_V+1):(N-2)],f,n_H[2]+1)
    beta<-theta[N_U+N_V+N_W+c(1,2)] 
    
    Zeros1<-matrix(0,n,n_H[1])
    A1<-X%*%t(U)
    Z1<-cbind(rep(1,n),pmax(Zeros1,A1)) # size(n,n_H[1]+1)
    Zeros2<-matrix(0,n,n_H[2])
    A2<-Z1%*%t(V)
    Z2<-cbind(rep(1,n),pmax(Zeros2,A2)) # size(n,n_H[2]+1)
    alpha<-Z2%*%t(W)
    mass<-exp(alpha-apply(alpha,1,max))
    mass<-mass/rowSums(mass)
    if(is.null(fhat)) gam<-rep(0,n) else{
      #   eta<-pmax(0,beta[1]+beta[2]*fhat)
      eta<-log(1+exp(beta[1]+beta[2]*fhat))
      gam<-eta/(1+eta)
      mass<-cbind(gam+(1-gam)*mass[,1],matrix(1-gam,n,f-1)*mass[,2:f])
    }
    K<-matrix(0,n,p)
    for(i in 1:n) K[i,]=mass[i,] %*% Xi %*% t(mass[J[i,],])
  } # if 2 HL
  
 
  clus<-extractMass(mass=mass,F=F,method="nn-evclus",crit=Sbest,Kmat=K,
                        trace=trbest,D=D,J=J,
                     param=list(U=U,V=V,W=W,beta=beta))
  return(clus)
}
