#' @name DPC
#' @title Distributed principal component
#' @param data is a total data set
#' @param m is the number of principal component
#' @param n1 is  the length of each data subset
#' @param K is the number of nodes
#' @return Ahat,Dhat,Sigmahathat
#' @export
#' @examples
#' library(LFM)
#' data_from_package <- Wine
#' data_a <- Wine
#' DPC(data_a,m=3,n1=128,K=2)
#' @importFrom stats cor cov
DPC=function(data,m,n1,K){
  n<-nrow(data)
  p=ncol(data)
  X1=matrix(rep(0,n1*p),ncol=p)
  Sigmahat=list()
  Ahat=list()
  Dhat=list()
  for (i in 1:K) {
    L=matrix(rep(0,K*n1),ncol=n1)
    R=matrix(0,n1,n)
    L[i,]=sample(1:n,n1,replace=FALSE)
    r=matrix(c(1:n1,L[i,]),ncol=n1,byrow=T)
    R[t(r)]=1
    X1=R%*%as.matrix(data)
    X=scale(X1)
    Sigmahat[[i]]<-cor(X)
    eig<-eigen(Sigmahat[[i]])
    lambdahat= eig$values[1:m]
    ind<-order(lambdahat,decreasing=T)
    lambdahat<-lambdahat[ind]
    Q<- eig$vectors
    Q<-Q[,ind]
    Qhat<-Q[,1:m]
    Ahat1 <- matrix(0, nrow = p, ncol = m)
    for (j in 1:m) {Ahat1[, j] <- sqrt(lambdahat[j]) * Qhat[, j]};
    Ahat[[i]] =Ahat1
    h0 <- diag(Ahat[[i]] %*% t(Ahat[[i]]))
    Dhat[[i]]<- diag(Sigmahat[[i]] - h0)
    S2=Ahat[[i]] %*% t(Ahat[[i]])+Dhat[[i]]}
  return(
    list(Ahat=Ahat,Dhat=Dhat,Sigmahat=Sigmahat))}
