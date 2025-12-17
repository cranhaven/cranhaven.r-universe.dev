#' @name PC
#' @title Principal component
#' @param data is a total data set
#' @param m is the number of principal component
#' @return Ahat, Dhat, Sigmahat
#' @export
#' @examples
#' library(LFM)
#' data_from_package <- Wine
#' data_a <- Wine
#' PC(data_a,m=5)
#' @importFrom LFM LFM
PC=function(data,m){
  X<-scale(data)
  p<-ncol(X)
  n<-nrow(X)
  Sigmahat<-cov(X)
  eig<-eigen(Sigmahat)
  lambdahat= eig$values[1:m]
  ind<-order(lambdahat,decreasing=T)
  lambdahat<-lambdahat[ind]
  Q<- eig$vectors
  Q<-Q[,ind]
  Qhat<-Q[,1:m]
  Ahat <- matrix(0, nrow = p, ncol = m)
  for (j in 1:m) {Ahat[, j] <- sqrt(lambdahat[j]) * Qhat[, j]}; Ahat
  h0 <- diag(Ahat %*% t(Ahat))
  Dhat<- diag(Sigmahat - h0)
  S2=Ahat %*% t(Ahat)+Dhat
  return(
    list(Ahat=Ahat,Dhat=Dhat,Sigmahat=Sigmahat))}
