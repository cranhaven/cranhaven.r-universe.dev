#' @name SPC
#' @title Apply the SPC method to the Laplace factor model
#' @description This function performs Sparse Principal Component Analysis (SPC) on the input data. It estimates factor loadings and uniquenesses while calculating mean squared errors and loss metrics for comparison with true values.
#'
#' @param data The data used in the SPC analysis.
#' @param m is the number of principal component
#' @param gamma is a sparse parameter
#' @return A list containing:
#' \item{As}{Estimated factor loadings, a matrix of estimated factor loadings from the SPC analysis.}
#' \item{Ds}{Estimated uniquenesses, a vector of estimated uniquenesses corresponding to each variable.}
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
#' results <- SPC(data, m, gamma=0.03)
#' print(results)
#' @export
#' @importFrom elasticnet spca
#' @importFrom matrixcalc frobenius.norm
SPC<-function(data,m,gamma){
  X<-scale(data)
  R<-cor(X)
  S<-R
  eig<-eigen(S)
  p<-nrow(S)
  n<-nrow(X)
  paras<-rep(gamma,1*m,m)
  spc<-spca(R,K=m,type="Gram",sparse="penalty",trace=FALSE,para=paras)
  lambda<-eig$values[1:m]
  V<-spc$loadings[,1:m]
  As<-matrix(0,nrow=p,ncol=m)
  for (j in 1:m){
    As[,j]<-sqrt(lambda[j])*V[,j]
  }
  As;table(As==0)
  h2<-diag(As%*%t(As))
  Ds<-diag(S-h2)
  return(list(As=As,Ds=Ds))
}
