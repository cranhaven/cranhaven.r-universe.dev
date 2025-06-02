#' K nearest neighbors in a dissimilarity matrix
#'
#'\code{knn_dist} searches for nearest neighbors in a dissimilarity matrix matrix.
#'
#' This function is called by \code{\link{EkNNclus}} if argument x is not supplied.
#' It is not optimized and cannot be used for very large D. If an attribute matrix
#' x is supplied and D is the matrix of Euclidean distances, it is preferable to use
#' function \code{\link[FNN]{get.knn}} from package \code{FNN}.
#'
#' @param D Dissimilarity matrix of size (n,n), where n is the number of objects.
#' @param K Number of neighbors
#'
#' @return A list with two components:
#' \describe{
#'   \item{nn.dist}{An (n,K) matrix for the nearest neighbor dissimilarities.}
#'   \item{nn.index}{An (n,K) matrix for the nearest neighbor indices.}
#' }
#'
#'@author Thierry Denoeux.
#'
#' @export
#'
#' @seealso \code{\link[FNN]{get.knn}}, \code{\link{EkNNclus}}
#'
#' @examples
#' data(butterfly)
#' n <- nrow(butterfly)
#' D<-as.matrix(dist(butterfly))
#' knn<-knn_dist(D,K=2)
#' knn$nn.dist
#' knn$nn.index
knn_dist<- function(D,K){
  n<-dim(D)[1]
  knn<-matrix(0,nrow=n,ncol=K)
  index<-knn
  for(i in (1:n)){
    ds<-sort(D[i,],index.return=TRUE)
    knn[i,]<-ds$x[2:(K+1)]
    index[i,]<-ds$ix[2:(K+1)]
  }
  return(list(nn.dist=knn,nn.index=index))
}
