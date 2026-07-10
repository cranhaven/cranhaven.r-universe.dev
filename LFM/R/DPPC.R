#' @name DPPC
#' @title Distributed projection principal component
#' @param data is a total data set
#' @param m is the number of principal component
#' @param n1 is  the length of each data subset
#' @param K is the number of nodes
#' @return Apro,pro,Sigmahathatpro
#' @export
#' @examples
#' library(LFM)
#' data_from_package <- Wine
#' data_a <- Wine
#' DPPC(data_a,m=3,n1=128,K=2)
#' @importFrom stats cor cov
DPPC=function(data,m,n1,K){
  n=nrow(data)
  p=ncol(data)
  P=as.matrix(diag(c(0,1),n,n))
  X1pro=scale(P%*%as.matrix(data))
  Sigmahatpro=list()
  Apro=list()
  Dpro=list()
  for (i in 1:K) {
    L=matrix(rep(0,K*n1),ncol=n1)
    R=matrix(0,n1,n)
    L[i,]=sample(1:n,n1,replace=FALSE)
    r=matrix(c(1:n1,L[i,]),ncol=n1,byrow=T)
    R[t(r)]=1
    X1=R%*%X1pro
    Xpro=scale(X1)
    Sigmahatpro[[i]]<-cor(Xpro)
    eig1<-eigen(Sigmahatpro[[i]])
    lambdahat =eig1$values[1:m]
    ind<-order(lambdahat,decreasing=T)
    lambdahat<-lambdahat[ind]
    Q <- eig1$vectors
    Q<-Q[,ind]
    Qhat<-Q[,1:m]
    Apro1 <- matrix(0, nrow = p, ncol = m)
    for (j in 1:m) {Apro1[, j] <- sqrt(lambdahat[j]) * Qhat[, j]}
    Apro[[i]] =Apro1
    hpro <- diag(Apro[[i]] %*% t(Apro[[i]]))
    Dpro[[i]] <- diag(Sigmahatpro[[i]] - hpro) }
  return(list(Apro=Apro,Dpro=Dpro,Sigmahatpro=Sigmahatpro))}
