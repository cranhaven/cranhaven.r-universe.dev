#' Computation of a  Euclidean distance matrix
#'
#' \code{createD} constructs an n x k matrix of Euclidean distances from an n x p matrix
#' of attribute data. For each object, the distances to k randomly selected objects are
#' computed.
#'
#' @param x n x p data matrix.
#' @param k Number of distances. If missing, an n x n distance matrix is computed.
#'
#' @return A list with two elements:
#' \describe{
#' \item{D}{n x k distance matrix.}
#' \item{J}{n x k matrix of indices. D[i,j] is the Euclidean distance between x[i,] and
#' x[J[i,j],].}
#' }
#' @export
#' @importFrom stats dist
#'
#' @seealso \code{\link{kevclus}}
#'
#' @examples
#' data(fourclass)
#' x<-as.matrix(fourclass[,1:2])
#' dist<-createD(x,k=10)
#' dim(dist$D)
#' dim(dist$J)
#'
createD<- function(x,k){
  if(missing(k)){
    D<-as.matrix(dist(x))
    J<-NULL
  }
  else{
    n<-nrow(x)
    p<-ncol(x)
    D<-matrix(0,n,k)
    J<-D
    for(i in 1:n){
      ii<-sample((1:n)[-i],k)
      J[i,]<-ii
      D[i,]<-sqrt(rowSums((x[ii,]-matrix(x[i,],k,p,byrow=TRUE))^2))
    }
  }
  return(list(D=D,J=J))
}
