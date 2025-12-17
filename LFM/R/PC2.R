#' @name PC2
#' @title Apply the PC method to the Laplace factor model
#' @description This function performs principal component analysis (PCA) on a given data set to reduce dimensionality. It calculates the estimated values for the loadings, specific variances, and the covariance matrix.
#' @param data The total data set to be analyzed.
#' @param m The number of principal components to retain in the analysis.
#' @return Ahat,Dhat,Sigmahat
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
#' results <- PC2(data, m)
#' print(results)
#' @export
#' @importFrom matrixcalc frobenius.norm
#' @importFrom stats cor
PC2=function(data,m){
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

