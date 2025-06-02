#' Pairwise Constrained Component Analysis (PCCA)
#'
#' Using must-link and cannot-link constaints, PCCA (Mignon & Jury, 2012) learns a projection into a 
#' low-dimensional space where the distances between pairs of data points respect the desired constraints, 
#' exhibiting good generalization properties in presence of high dimensional data.
#'
#' @param x Data matrix of size n*d
#' @param d1 Number of extracted features.
#' @param ML Matrix nbML x 2 of must-link constraints. Each row of ML contains the indices
#' of objects that belong to the same class.
#' @param CL Matrix nbCL x 2 of cannot-link constraints. Each row of CL contains the indices
#' of objects that belong to different classes.
#' @param options Parameters of the optimization algorithm (see \code{\link{harris}}).
#' @param beta Sharpness parameter in the loss function (default: 1).
#'
#' @return A list with three attributes:
#'  \describe{
#'   \item{z}{The n*d1 matrix of extracted features.}
#'   \item{L}{The projection matrix of size d1*d.}
#'   \item{D}{The Euclidean distance matrix in the projected space}
#'  }
#'
#'@references A. Mignon and F. Jurie. PCCA: a new approach for distance learning from sparse 
#'pairwise constraints. In 2012 IEEE Conference on Computer Vision and Pattern Recognition, 
#'pages 2666-2672, 2012.
#'
#' @author Thierry Denoeux.
#'
#' @export
#'
#' @seealso \code{\link{kpcca}},\code{\link{harris}},\code{\link{create_MLCL}}
#' 
#' @examples 
#'\dontrun{
#' data(iris)
#' x<-as.matrix(iris[,1:4])
#' y<-as.integer(iris[,5])
#' const<-create_MLCL(y,50)
#' res.pcca<-pcca(x,1,const$ML,const$CL)
#' plot(res.pcca$z,col=y,pch=y)
#' }
#'
pcca<-function(x,d1,ML,CL,options=c(1,1000,1e-5,10),beta=1){
  nml<-nrow(ML)
  ncl<-nrow(CL)
  d<-ncol(x)
  nc<-nml+ncl
  Xi<-rbind(x[ML[,1],],x[CL[,1],])
  Xj<-rbind(x[ML[,2],],x[CL[,2],])
  y<-c(rep(1,nml),rep(-1,ncl))
  C<-array(0,c(nc,d,d))
  for(k in 1:nc) C[k,,]<-(Xi[k,]-Xj[k,])%*%t(Xi[k,]-Xj[k,])
  l<-rnorm(d*d1,sd=0.001)
  opt<-harris(fg.pcca,l,Xi=Xi,Xj=Xj,y=y,C=C,beta=beta,options=options)
  L<-matrix(opt$par,d1,d)
  z<-x%*%t(L)
  D<-as.matrix(dist(z))
  return(list(L=L,z=z,D=D))
}





