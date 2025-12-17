#' @name PC1
#' @title Apply the traditional principal component method to the Laplace factor model
#' @description This function performs Principal Component Analysis (PCA) on a given data set to reduce dimensionality. It calculates the estimated values for the loadings, specific variances, and the covariance matrix.
#' @param data The total data set to be analyzed.
#' @param m The number of principal components to retain in the analysis.
#' @return Ahat, Dhat
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
#' results <- PC1(data, m)
#' print(results)
#' @export
#' @importFrom matrixcalc frobenius.norm
#' @importFrom stats cov
PC1<-function(data,m=m){
  X<-scale(data)
  R<-cor(X)
  S<-R
  eig<-eigen(S)
  p<-nrow(S)
  diag_S<-diag(S)
  sum_rank<-sum(diag_S)
  rowname<-paste("X",1:p,sep="")
  colname<-paste("Factor",1:m,sep="")
  Ahat<-matrix(0,nrow=p,ncol=m,
               dimnames=list(rowname,colname))
  rowname<-c("SS loadings","Proportion Var","Cumulative Var")
  for (i in 1:m){
    Ahat[,i]<-sqrt(eig$values[i])*eig$vectors[,i]
  }
  h2<-diag(Ahat%*%t(Ahat))
  Dhat<-diag(S-h2)
  return(list(Ahat=Ahat,Dhat=Dhat))
}

