#' Kernel Pairwise Constrained Component Analysis (KPCCA)
#'
#' Using must-link and cannot-link constaints, KPCCA (Mignon & Jury, 2012) learns a projection into a 
#' low-dimensional space where the distances between pairs of data points respect the desired constraints, 
#' exhibiting good generalization properties in presence of high dimensional data. This is a kernelized
#' version of \code{pcca}.
#'
#' @param K Gram matrix of size n*n
#' @param d1 Number of extracted features.
#' @param ML Matrix nbML x 2 of must-link constraints. Each row of ML contains the indices
#' of objects that belong to the same class.
#' @param CL Matrix nbCL x 2 of cannot-link constraints. Each row of CL contains the indices
#' of objects that belong to different classes.
#' @param beta Sharpness parameter in the loss function (default: 1).
#' @param epsi Minimal rate of change of the cost function (default: 1e-4).
#' @param etamax Maximum step in the line search algorithm (default: 0.1).
#' @param disp If TRUE (default), intermediate results are displayed.
#'
#' @return A list with three attributes:
#'  \describe{
#'   \item{z}{The n*d1 matrix of extracted features.}
#'   \item{A}{The projection matrix of size d1*n.}
#'   \item{D}{The Euclidean distance matrix in the projected space.}
#'  }
#'
#'@references A. Mignon and F. Jurie. PCCA: a new approach for distance learning from sparse 
#'pairwise constraints. In 2012 IEEE Conference on Computer Vision and Pattern Recognition, 
#'pages 2666-2672, 2012.
#'
#' @author Thierry Denoeux.
#'
#' @export
#' @importFrom stats optimize
#'
#' @seealso \code{\link{pcca}}, \code{\link{create_MLCL}}
#' 
#' @examples 
#'\dontrun{
#' library(kernlab)
#' data<-bananas(400)
#' plot(data$x,pch=data$y,col=data$y)
#' const<-create_MLCL(data$y,1000)
#' rbf <- rbfdot(sigma = 0.2)
#' K<-kernelMatrix(rbf,data$x)
#' res.kpcca<-kpcca(K,d1=1,ML=const$ML,CL=const$CL,beta=1)
#' plot(res.kpcca$z,col=data$y)
#' }
#'
kpcca<-function(K,d1,ML,CL,beta=1,epsi=1e-4,etamax=0.1,disp=TRUE){
  
  # Loss function
  fonc<-function(eta,A,G,DKij,y,beta) fun<-loss(A-eta*G,DKij,y,beta)
  loss<-function(A,DKij,y,beta){
    D2<-colSums((A%*%t(DKij))^2)
    toto<-beta*y*(D2-1)
    fun<-sum(log(1+exp(toto)))/beta
  }
  
  # version with line search
  nml<-nrow(ML)
  ncl<-nrow(CL)
  n<-ncol(K)
  nc<-nml+ncl
  MCL<-rbind(ML,CL)
  DKij<-K[MCL[,1],]-K[MCL[,2],]
  y<-c(rep(1,nml),rep(-1,ncl))
  KJ<-array(0,c(nc,n,n))
  E<-diag(n)
  for(k in 1:nc) KJ[k,,]<-K%*%(E[MCL[k,1],]-E[MCL[k,2],])%*%t(E[MCL[k,1],]-E[MCL[k,2],])
  A<-matrix(rnorm(n*d1,sd=0.001),d1,n)
  go_on<-TRUE
  t<-0
  f0<-loss(A,DKij=DKij,y=y,beta=beta)
  if(disp) print(c(0,f0))
  while(go_on){
    t<-t+1
    D2<-colSums((A%*%t(DKij))^2)
    L<-y/(1+exp(beta*y*(1-D2)))
    G<-2*A %*% apply(KJ,c(2,3),function(KJn) sum(L* KJn))
    opt<-optimize(f=fonc,c(0,etamax),A=A,G=G,DKij=DKij,y=y,beta=beta)
    eta<-opt$minimum
    A<-A-eta*G
    Df<-(f0-opt$objective)/f0
    if((Df<epsi)&(t>10)) go_on<-FALSE
    f0<-opt$objective
    if (disp) print(c(t,f0,Df))
  }
  z<-K%*%t(A)
  D<-as.matrix(dist(z))
  return(list(A=A,z=z,D=D))
}



