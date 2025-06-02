#' Finding overlapping pairs of clusters
#'
#' \code{createPairs} finds pairs of clusters that are mutual k nearest neighbors
#' in a credal partition. The similarity between two clusters k and l is defined as
#' \eqn{\sum_{i=1}^n pl_{ik} pl_{il}}, where \eqn{pl_{ik}} is the plausibility of object i belonging to
#' cluster k.
#'
#' This function allows one to use evidential clustering when the number of clusters
#' is large. A clustering algorithm is first run with a limited number
#' of focal sets (the empty set, the singletons and, optionally, the whole frame). Then,
#' the similarity between clusters is analysed to determine the pairs of neighboring
#' (overlapping) clusters. The clustering algorithm is then run again, adding these
#' pairs to the focal sets (see the example). The focal sets of the passed credal partition must be
#' the empty set (first row), the singletons (next c rows) and, optionally, the whole frame
#' (last row).
#'
#' @param clus An object of class \code{credpart}. It should contain at
#' least two fields: clus$mass (the credal partition) and clus$pl.n (the normalized
#' plausibilities). The focal sets of the credal partition must be the empty set,
#' the singletons, and (optionally) the whole set of clusters.
#' @param k The number of neighbors.
#'
#' @return A list with the following components:
#' \describe{
#' \item{pairs}{A matrix with two columns and p rows, containing the p pairs of
#' clusters. This matrix can be passed to \code{\link{ecm}}, \code{\link{recm}},
#' \code{\link{cecm}} or \code{\link{kevclus}}.}
#' \item{m0}{A  matrix of size (n,c+2+p), encoding the credal partition. The masses
#' assigned to the pairs are null.}
#' \item{S}{The c x c matrix of similarities between clusters.}
#' }
#' @export
#'
#' @seealso \code{\link{extractMass}}, \code{\link{ecm}}, \code{\link{recm}},
#'\code{\link{cecm}}, \code{\link{kevclus}}.
#'
#' @references
#'T. Denoeux, S. Sriboonchitta and O. Kanjanatarakul. Evidential clustering of large
#'dissimilarity data. Knowledge-Based Systems, vol. 106, pages 179-195, 2016.
#'
#' @examples
#' ## Example with Four-class data
#' data("fourclass")
#' x<-fourclass[,1:2]
#' y<-fourclass[,3]
#' c=4
#' ## Running k-EVCLUS with singletons
#' clus<-kevclus(x=x,k=100,c=c,type='simple')
#' ## Plot the results
#' plot(clus,X=x,mfrow=c(2,2),ytrue=y)
#' ## Creating the pairs of clusters
#' P<-createPairs(clus,k=2)
#' ## Running k-EVCLUS again, with pairs of clusters
#' clus1<-kevclus(x=x,k=100,c=c,type='pairs',pairs=P$pairs,m0=P$m0)
#' ## Plot the results
#' plot(clus1,X=x,mfrow=c(2,2),ytrue=y)
createPairs<- function(clus,k=1){
  c<-ncol(clus$pl.n)
  n<-nrow(clus$pl.n)
  f<-nrow(clus$F)
  if((f<c+1) | (f>c+2)){
    print('Error: the number of focal sets must be c+1 or c+2.')
    return()
  }
  S=t(clus$pl.n)%*%clus$pl.n
  P<-matrix(FALSE,c,c)
  for(i in (1:c)){
    ds<-sort(S[i,],decreasing=TRUE,index.return=TRUE)
    P[i,ds$ix[2:(k+1)]]<-TRUE
  }
  P<-(P & t(P)) & upper.tri(P)

  pairs<-which(P,arr.ind=TRUE)
  npairs<-nrow(pairs)

  m0<-cbind(clus$mass[,1:(c+1)],matrix(0,n,npairs))
  if(f == (c+2)) m0<-cbind(m0,clus$mass[,c+2])
  return(list(pairs=pairs,m0=m0,S=S))
}
