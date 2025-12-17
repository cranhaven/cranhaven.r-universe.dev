#' @name GulPC
#' @title Apply the GulPC method to the Laplace factor model
#' @description This function performs General Unilateral Loading Principal Component (GulPC) analysis on a given data set. It calculates the estimated values for the first layer and second layer loadings, specific variances, and the mean squared errors.
#' @param data A matrix of input data.
#' @param m is the number of first layer principal component
#' @return AU1,AU2,DU3,SigmaUhat
#' @examples
#' library(LaplacesDemon)
#' library(MASS)
#' n=1000
#' p=10
#' m=5
#' mu=t(matrix(rep(runif(p,0,1000),n),p,n))
#' mu0=as.matrix(runif(m,0))
#' sigma0=diag(runif(m,1))
#' F=matrix(mvrnorm(n,mu0,sigma0),nrow=n)
#' A=matrix(runif(p*m,-1,1),nrow=p)
#' lanor <- rlaplace(n*p,0,1)
#' epsilon=matrix(lanor,nrow=n)
#' D=diag(t(epsilon)%*%epsilon)
#' data=mu+F%*%t(A)+epsilon
#' results <- GulPC(data, m)
#' print(results)
#' @export
#' @importFrom matrixcalc frobenius.norm
#' @importFrom stats cor
GulPC=function(data,m){
  X=scale(data)
  n=nrow(X)
  p<-ncol(X)
  SigmaU1hat=cor(X)
  eig5<-eigen(SigmaU1hat)
  lambda1hat = eig5$values[1:m]
  ind<-order(lambda1hat,decreasing=T)
  lambda1hat<-lambda1hat[ind]
  Q1<-eig5$vectors
  Q1=Q1[,ind]
  Q1hat<- Q1[, 1:m]
  AU1 <- matrix(0, nrow = p, ncol = m)
  for (j in 1:m) {AU1[, j] <- sqrt(lambda1hat[j]) * Q1hat[, j]}; AU1
  hU1 <- diag(AU1 %*% t(AU1))
  DU1 <- diag(SigmaU1hat - hU1)
  pc=2
  F1hat=X%*%AU1
  F1star<-F1hat/sqrt(n)
  SigmaU2hat=cov(F1star)
  eig6<-eigen(SigmaU2hat)
  lambda2hat =eig6$values[1:pc]
  ind<-order(lambda2hat,decreasing=T)
  lambda2hat<-lambda2hat[ind]
  Q2<-eig6$vectors
  Q2=Q2[,ind]
  Q2hat<- Q2[, 1:pc]
  AU2<- matrix(0, nrow = m, ncol = pc)
  for (j in 1:pc) {AU2[, j] <- sqrt(lambda2hat[j]) * Q2hat[, j]}; AU2
  hU2 <- diag(AU2%*% t(AU2))
  DU2 <- diag(SigmaU2hat - hU2)
  Fhat=F1star%*%AU2
  Xhat=Fhat%*%t(AU2)%*%t(AU1)
  S1hat=cov(Xhat)
  hU3 <- diag(t(t(AU2)%*%t(AU1))%*%(t(AU2)%*%t(AU1)))
  DU3 <- diag(S1hat - hU3)
  return(list(AU1=AU1,AU2=AU2,DU3=DU3,S1hat=S1hat))}

