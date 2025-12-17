#' @name SOPC
#' @title Apply the SOPC method to the Laplace factor model
#' @description This function calculates various metrics for the sprase online principle component (SOPC) analysis
#' on the Laplace factor model.
#' @param data A numeric matrix containing the data used in the SOPC analysis.
#' @param m is the number of principal component
#' @param gamma is a sparse parameter
#' @param eta is the proportion of online data to total data
#' @usage SOPC(data, m, gamma,eta)
#' @return Aso,Dso
#' @examples
#' library(MASS)
#' library(matrixcalc)
#' library(LaplacesDemon)
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
#' results <- SOPC(data, m, gamma=0.03, eta=0.1)
#' print(results)
#' @export
#' @importFrom matrixcalc frobenius.norm
SOPC<-function(data,m,gamma,eta){
  X<-scale(data)
  S<-cov(X)
  eig<-eigen(S)
  p<-nrow(S)
  n<-nrow(X)
  n0<-round(eta*n)
  Xbar<-colMeans(X[1:n0,])
  S0<-cov(X[1:n0,])
  lambda=eigen(S0)$values[1:m]
  V<-eigen(S0)$vectors[,1:m]
  paras<-rep(gamma,1*m,m)
  Sd<-S0

  iter1=0
  for (i in (n0+1):n) {
    iter1=iter1+1
    Xcenter<-t(X[i,])
    Sd<-((i-1)/i)*Sd+(1/i)*t(Xcenter)%*%Xcenter
    lambda<-eigen(Sd)$values
    spc<-spca(Sd,K=m,type="Gram",max.iter=0,sparse="penalty",trace=FALSE,para=paras)
    V<-spc$loadings
  }
  lambda2<-lambda[1:m]
  V2<-V[,1:m]
  Aso<-matrix(0,nrow=p,ncol=m)
  for (j in 1:m){
    Aso[,j]<-sqrt(lambda2[j])*V2[,j]
  }
  Aso;table(Aso==0)
  h2<-diag(Aso%*%t(Aso))
  Dso<-diag(S-h2)
  return(list(Aso=Aso,Dso=Dso))
}

