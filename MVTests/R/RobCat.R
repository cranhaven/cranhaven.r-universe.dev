#' @title Robust CAT Algorithm
#'
#' @description
#' \code{RobCat} computes p value based on robust CAT algorithm to compare two means vectors 
#' under multivariate Behrens-Fisher problem.
#'
#' @details
#' This function computes p value based on robust CAT algorithm to compare two means vectors 
#' under multivariate Behrens-Fisher problem. When p value<0.05, it means the difference of two mean vectors is significant statistically.
#' 
#' @param X a matrix or data frame for first group.
#' @param Y a matrix or data frame for second group.
#' @param M iteration number and the default is 1000.
#' @param alpha numeric parameter controlling the size of the subsets over which the determinant is minimized; roughly alpha*n, observations are used for computing the determinant. Allowed values are between 0.5 and 1 and the default is 0.75. 
#' @export
#' @return a list with 2 elements:
#' \item{Cstat}{Calculated value of test statistic }
#' \item{pval}{The p value}
#' @author Hasan BULUT <hasan.bulut@omu.edu.tr>
#' @examples
#' 
#' data(iris)
#' if (requireNamespace("robustbase", quietly=TRUE)) {
#' RobCat(X=iris[1:20,-5],Y=iris[81:100,-5])}

RobCat<-function(X,Y,M=1000,alpha=0.75) {
  if (!requireNamespace("robustbase", quietly=TRUE))
    stop("Package 'robustbase' is required for RobCat (covMcd). Please install it.", call.=FALSE)
  
  
  result1<-robustbase::covMcd(X,alpha=alpha)
  result2<-robustbase::covMcd(Y,alpha=alpha)
  
  x.mean<-result1$center   
  y.mean<-result2$center
  
  sx<- result1$cov      
  sy<-result2$cov
  
  h1<-length(result1$best)  ; p<-ncol(X)
  h2<-length(result2$best)
  
  mu0<-solve(solve(sx)*h1+solve(sy)*h2)%*%(h1*solve(sx)%*%x.mean+h2*solve(sy)%*%y.mean)
  
  pay1<-matrix(0,nrow=p,ncol=p)
  for(i in result1$best){
    dif<-as.matrix(X[i,]-mu0)
    pay1<-pay1+t(dif)%*%dif
  }
  
  Sigma1.hat<-pay1/h1
  
  pay2<-matrix(0,nrow=p,ncol=p)
  for(i in result2$best){
    dif<-as.matrix(Y[i,]-mu0)
    pay2<-pay2+t(dif)%*%dif
  }
  
  Sigma2.hat<-pay2/h2
  
  DIF<-as.matrix(x.mean-y.mean)
  sigma.pool<-((Sigma1.hat/h1)+(Sigma2.hat/h2))
  Cat.val<- t(DIF)%*%solve(sigma.pool)%*%DIF
  
  muk<-mu0
  k=1
  while (k<250) {
    
    pay1<-matrix(0,nrow=p,ncol=p)
    for(i in result1$best){
      dif<-as.matrix(X[i,]-muk)
      pay1<-pay1+t(dif)%*%dif
    }
    
    Sigma1k<-pay1/h1
    
    pay2<-matrix(0,nrow=p,ncol=p)
    for(i in result2$best){
      dif<-as.matrix(Y[i,]-muk)
      pay2<-pay2+t(dif)%*%dif
    }
    
    Sigma2k<-pay2/h2
    
    mukminus1<-muk
    muk<-solve(solve(Sigma1k)*h1+solve(Sigma2k)*h2)%*%(h1*solve(Sigma1k)%*%x.mean+h2*solve(Sigma2k)%*%y.mean)
    
    kst=0
    for(i in 1:p){
      if(abs(muk[i]-mukminus1[i])<0.0001){
        kst=kst+1  
      }}
    
    if(kst==p){
      break
    }
    k=k+1
  }
  
  MU.RML<-muk
  Sigma1.RML<-Sigma1k
  Sigma2.RML<-Sigma2k
  
  Cat.ML<-NULL
  for(i in 1:M){
    data1<-mvtnorm::rmvnorm(n=h1,mean=MU.RML,sigma=Sigma1.RML)
    data2<-mvtnorm::rmvnorm(n=h2,mean=MU.RML,sigma=Sigma2.RML)
    
    x.mean<-colMeans(data1)  ; y.mean<-colMeans(data2) 
    dif<-(x.mean-y.mean)
    sigma.pool<-((Sigma1.RML/h1)+(Sigma2.RML/h2))
    Cat.ML[i]<-t(dif)%*%solve(sigma.pool)%*%dif     
    
  }
  
  pval<-length(which(Cat.ML>rep(Cat.val,M)))/M
  return(list(Cstat=Cat.val,pval=pval))
  
}
