#' @name PPC1
#' @title Apply the PPC method to the Laplace factor model
#' @description This function computes Perturbation Principal Component Analysis (PPC) for the provided input data, estimating factor loadings and uniquenesses. It calculates mean squared errors and loss metrics for the estimated values compared to true values.
#' @param data A matrix of input data.
#' @param m The number of principal components.
#' @return Apro,Dpro,Sigmahatpro
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
#' results <- PPC1(data, m)
#' print(results)
#' @export
#' @importFrom matrixcalc frobenius.norm
PPC1=function(data,m){
  X=scale(data)
  n=nrow(X)
  p=ncol(X)
  P=as.matrix(diag(c(0,1),n,n))
  Xpro=scale(P%*%X)
  Sigmahatpro<-cov(Xpro)
  eig<-eigen(Sigmahatpro)
  lambdahat =eig$values[1:m]
  ind<-order(lambdahat,decreasing=T)
  lambdahat<-lambdahat[ind]
  Q <- eig$vectors
  Q<-Q[,ind]
  Qhat<-Q[,1:m]
  Apro <- matrix(0, nrow = p, ncol = m)
  for (j in 1:m) {Apro[, j] <- sqrt(lambdahat[j]) * Qhat[, j]}; Apro
  hpro <- diag(Apro %*% t(Apro))
  Dpro <- diag(Sigmahatpro - hpro)
  return(list(Apro=Apro,Dpro=Dpro,Sigmahatpro=Sigmahatpro))}
