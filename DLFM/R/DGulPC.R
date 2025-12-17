#' @name DGulPC
#' @title Distributed general unilateral loading principal component
#' @param data is a total data set
#' @param m is the number of principal component
#' @param n1 is  the length of each data subset
#' @param K is the number of nodes
#' @return AU1,AU2,DU3,Shat
#' @export
#' @examples
#' library(LFM)
#' data_from_package <- Wine
#' data_a <- Wine
#' DGulPC(data_a,m=3,n1=128,K=2)
#' @importFrom stats cor cov
DGulPC=function(data,m,n1,K){
  SigmaU1hat=list()
  SigmaU2hat=list()
  AU1=list()
  AU2=list()
  DU3=list()
  shat=list()
  Fhat=list()
  F1hat=list()
  DU1=list()
  DU2=list()
  for (i in 1:K) {
    n=nrow(data)
    p=ncol(data)
    pc=2
    L=matrix(rep(0,K*n1),ncol=n1)
    R=matrix(0,n1,n)
    L[i,]=sample(1:n,n1,replace=FALSE)
    r=matrix(c(1:n1,L[i,]),ncol=n1,byrow=T)
    R[t(r)]=1
    X1=R%*%as.matrix(data)
    X=scale(X1)
    SigmaU1hat[[i]]<-cor(X)
    eig5<-eigen(SigmaU1hat[[i]])
    lambda1hat = eig5$values[1:m]
    ind<-order(lambda1hat,decreasing=T)
    lambda1hat<-lambda1hat[ind]
    Q1<-eig5$vectors
    Q1=Q1[,ind]
    Q1hat<- Q1[, 1:m]
    AU11 <- matrix(0, nrow = p, ncol = m)
    for (j in 1:m) {AU11[, j] <- sqrt(lambda1hat[j]) * Q1hat[, j]}
    AU1[[i]]=AU11
    hU1 <- diag(AU1[[i]] %*% t(AU1[[i]]))
    DU1[[i]] <- diag(SigmaU1hat[[i]] - hU1)
    p<-ncol(X)
    F1hat[[i]]=X%*%AU1[[i]]
    F1star<-F1hat[[i]]/sqrt(n)
    SigmaU2hat[[i]]=cor(F1star)
    eig6<-eigen(SigmaU2hat[[i]])
    lambda2hat =eig6$values[1:pc]
    ind<-order(lambda2hat,decreasing=T)
    lambda2hat<-lambda2hat[ind]
    Q2<-eig6$vectors
    Q2=Q2[,ind]
    Q2hat<- Q2[, 1:pc]
    AU22<- matrix(0, nrow = m, ncol = pc)
    for (j in 1:pc) {AU22[, j] <- sqrt(lambda2hat[j]) * Q2hat[, j]}
    AU2[[i]]=AU22
    hU2 <- diag(AU2[[i]]%*% t(AU2[[i]]))
    DU2[[i]] <- diag(SigmaU2hat[[i]] - hU2)
    Fhat[[i]]=F1star%*%AU2[[i]]
    Xhat=Fhat[[i]]%*%t(AU2[[i]])%*%t(AU1[[i]])
    shat[[i]]=cov(Xhat)
    hU3 <- diag(t(t(AU2[[i]])%*%t(AU1[[i]]))%*%(t(AU2[[i]])%*%t(AU1[[i]])))
    DU3[[i]] <- diag(shat[[i]] - hU3)
  return(list(AU1=AU1,AU2=AU2,DU3=DU3,shat=shat))}}

