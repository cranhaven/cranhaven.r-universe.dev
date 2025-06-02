#' EkNNclus algorithm
#'
#'\code{EkNNclus} computes hard and credal partitions from dissimilarity or attribute
#'data using the EkNNclus algorithm.
#'
#' The number of clusters is not specified. It is influenced by parameters K and q.
#' (It is advised to start with the default values.) For n not too large (say, until one
#' thousand), y0 can be defined as the vector (1,2,...,n). For larger values of n, it is
#' advised to start with a random partition of c clusters, c<n.
#'
#' @param x n x p data matrix (n instances, p attributes).
#' @param D n x n dissimilarity matrix (used only if x is not supplied).
#' @param K Number of neighbors.
#' @param y0 Initial partition (vector of length n, with values in (1,2,...)).
#' @param ntrials Number of runs of the algorithm (the best solution is kept).
#' @param q  Parameter in (0,1). Gamma is set to the inverse of the q-quantile of distances
#' from the K nearest neighbors (same notation as in the paper).
#' @param b Exponent of distances, \eqn{\alpha_{ij} = \phi(d_{ij}^b)}.
#' @param disp If TRUE, intermediate results are displayed.
#' @param tr If TRUE, a trace of the cost function is returned.
#' @param eps Minimal distance between two vectors (distances smaller than \code{eps}
#' are replaced by \code{eps})

#'
#' @return The credal partition (an object of class \code{"credpart"}). In addition to the
#' usual attributes, the output credal partition has the following attributes:
#' \describe{
#'   \item{trace}{Trace of the algorithm (sequence of values of the cost function).}
#'   \item{W}{The weight matrix.}
#' }
#'
#'@references T. Denoeux, O. Kanjanatarakul and S. Sriboonchitta.
#'  EK-NNclus: a clustering procedure based on the evidential K-nearest neighbor rule.
#'  Knowledge-Based Systems, Vol. 88, pages 57--69, 2015.
#'
#'@author Thierry Denoeux.
#'
#' @export
#' @import FNN
#' @importFrom stats quantile
#'
#' @examples ## Clustering of the fourclass dataset
#' \dontrun{
#' data(fourclass)
#' n<-nrow(fourclass)
#' N=2
#' clus<- EkNNclus(fourclass[,1:2],K=60,y0=(1:n),ntrials=N,q=0.9,b=2,disp=TRUE,tr=TRUE)
#' ## Plot of the partition
#' plot(clus,X=fourclass[,1:2],ytrue=fourclass$y,Outliers=FALSE,plot_approx=FALSE)
#' ## Plot of the cost function vs number of iteration
#' L<-vector(length=N)
#' for(i in 1:N) L[i]<-dim(clus$trace[clus$trace[,1]==i,])[1]
#' imax<-which.max(L)
#' plot(0:(L[imax]-1),-clus$trace[clus$trace[,1]==imax,3],type="l",lty=imax,
#' xlab="time steps",ylab="energy")
#' for(i in (1:N)) if(i != imax) lines(0:(L[i]-1),-clus$trace[clus$trace[,1]==i,3],
#' type="l",lty=i)
#' }

EkNNclus <- function(x=NULL,D,K,y0,ntrials=1,q=0.5,b=1,disp=TRUE,tr=FALSE,
                     eps=1e-6)
{
  p<-b
  # Computation of nearest neighbors
  if(is.null(x)) nn<-knn_dist(D,K) else {
    x<-as.matrix(x)
    nn<-get.knn(x,K)
  }
 
  index<-nn$nn.index
  n<-dim(index)[1]
  nn$nn.dist<-pmax(matrix(eps,n,K),nn$nn.dist)
  
  # Computation of matrix W
  g<-1/quantile(nn$nn.dist^p,q)
  alpha<-pmax(exp(-g*nn$nn.dist^p),1e-6)
  W=-log(1-alpha)

  # Main algorithm
  critmax<- -1
  Trace<- NULL
  for(itrial in (1:ntrials)){
    nb.change<-1
    k<-0
    y<-y0
    c<-max(y)
    I<-diag(c)
    crit<-0
    for(i in (1:n)) crit<-crit+0.5*sum(W[i,]*(y[i]==y[index[i,]]))
    if(tr==TRUE) Trace<-rbind(Trace, c(itrial,k,crit,c))
    while(nb.change>0){
      k<-k+1
      ii<-sample.int(n)
      nb.change<-0
      for(i in (1:n)){
        S<-I[y[index[ii[i],]],]
        u<-W[ii[i],]%*%S
        kstar<-which.max(u)
        crit<-crit+u[kstar]-u[y[ii[i]]]
        if(y[ii[i]] != kstar){
          y[ii[i]]<-kstar
          nb.change<-nb.change+1
        } # end if
        if(tr==TRUE) Trace<-rbind(Trace, c(itrial,k,crit,c))
      } # end for i
      y<-simplify.labels(y)
      c<-max(y)
      I<-diag(c);
      if(disp==TRUE) print(c(itrial,k,nb.change,c))
    } # end while
    if(crit>critmax){
      critmax<-crit
      ybest<-y
    } #end if
  } # end for itrial
  c<-max(ybest)
  if(c>1){
    alpha<-1-exp(-W)
    m<-credal.partition(ybest,alpha,index)
    F<-makeF(c,type='simple')
    clus<-extractMass(m,F,method="EkNNclus",crit=critmax,trace=Trace,W=W)
  } else{
    clus<-NULL
    print("Error: degenerate solution (c=1)")
  }
  return(clus)
}







