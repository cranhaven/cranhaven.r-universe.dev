#' Delta-Bel graph for Belief Peak Evidential Clustering (BPEC)
#'
#'\code{delta_Bel} computes the delta-Bel graph used to determine the proptotypes in the
#' Belief Peak Evidential Clustering (BPEC) algorithm. The user must manually specify the rectangles 
#' containing the protytypes (which are typically in the upper-right corner of the graph is the clusters
#' are well-seperated). These prototypes are then used by function \code{bpec} to compute a credal 
#' partition.
#'
#' @param x input matrix of size n x d, where n is the number of objects and d the number of
#' attributes.
#' @param K Number of neighbors to determine belief values
#' @param q Parameter of the algorithm, between 0 and 1 (default: 0.9). 
#'
#' @return A list with three elements:
#' \describe{
#' \item{BelC}{The belief values.}
#' \item{delta}{The delta values.}
#' \item{g0}{A c*d matrix containing the prototypes.}
#' \item{ii}{List of indices of the belief peaks.}
#' }
#'
#'@references Z.-G. Su and T. Denoeux. BPEC: Belief-Peaks Evidential Clustering. IEEE Transactions 
#'on Fuzzy Systems, 27(1):111-123, 2019.
#'
#'@author Thierry Denoeux .
#'
#' @export
#' @importFrom graphics locator
#'
#' @seealso \code{\link{bpec}}
#'
#' @examples 
#' \dontrun{
#' data(fourclass)
#' x<-fourclass[,1:2]
#' y<-fourclass[,3]
#' DB<-delta_Bel(x,100,0.9)
#' plot(x,pch=".")
#' points(DB$g0,pch=3,col="red",cex=2)
#' }
delta_Bel<-function(x,K,q=0.9){
  n<-nrow(x)
  nn<-get.knn(x,K)
  D<-nn$nn.dist   
  is<-nn$nn.index
# calculate beliefs that possibly support itself as center
  alpha<-1/K
  if(is.null(q)) g<-rep(1,n) else g <- 1/apply(D,1,quantile,prob=q)
  BelC<-rep(0,n)
  for(i in 1:n) BelC[i]<-1-prod(1-alpha*exp(-g[is[i,]]^2*D[i,]^2))
# Calculation of delta
  Dist <- as.matrix(dist(x))
  maxdist<-max(Dist)
  tri<-sort(BelC,decreasing =TRUE,index.return = TRUE)
  bel_sorted<-tri$x
  ordbel<-tri$ix
  delta<-rep(0,n)
  delta[ordbel[1]]<- -1
  for(i in 2:n){
    delta[ordbel[i]]<-maxdist
    for(j in 1:(i-1)){
      if(Dist[ordbel[i],ordbel[j]]<delta[ordbel[i]]) delta[ordbel[i]]<-Dist[ordbel[i],ordbel[j]]
    }
  }
  delta[ordbel[1]]<-max(delta)
  
  # Plot of the delta-Bel graph
  plot(delta,BelC,ylim=range(min(BelC)-0.1,max(BelC)+0.1))
  print("With the mouse, select the lower left corner of a rectangle containing the prototypes.")
  t<-locator(1)
  abline(v=t$x,lty=2)
  abline(h=t$y,lty=2)
  ii<-which(delta>t$x & BelC>t$y)
  print(ii)
  points(delta[ii],BelC[ii],pch=3,col="red")
  g0<-x[ii,]
  
  
  return(list(BelC=BelC,delta=delta,g0=g0,ii=ii))
} 
  