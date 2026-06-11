Laplacian<- function(A){
  d<- apply(A,2,FUN="sum")
  L <- diag(1/sqrt(d))%*%A%*%diag(1/sqrt(d))
  return(L)
}