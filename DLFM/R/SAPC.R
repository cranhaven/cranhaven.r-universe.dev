#' The stochastic approximation principal component can handle online data sets with highly correlated.
#'
#' @param data is a highly correlated online data set
#' @param m is the number of principal component 
#' @param eta is the proportion of online data to total data
#'
#' @return Asa,Dsa
#' @export
#'
#' @examples
#' library(LFM)
#' data_from_package <- Wine
#' data_a <- Wine
#' SAPC(data=data_a,m=3,eta=0.8) 
SAPC<-function(data,m,eta){
  X<-as.matrix(scale(data))
  S<-cov(X)   
  n<-nrow(X)          
  n0<-round(eta*n)  
  p<-ncol(X)           
  Xbar<-colMeans(X[1:n0,])      
  eig1<-eigen(cov(X[1:n0,]-Xbar)) 
  lambda<-eig1$values[1:m]       
  V<-eig1$vectors[,1:m]      
  V1<-V
  
  for (i in (n0+1):n) {
    Xbar<-((i-1)/i)*Xbar+(1/i)*X[i,]
    Xcenter<-t(t(X[i,]-Xbar))
    gamma<-1/(i^(0.73))
    V<-V+gamma*Xcenter%*%t(Xcenter)%*%V
    V<-qr.Q(qr(V))
    lambda<- lambda+gamma*(t(V)%*%Xcenter%*%t(Xcenter)%*%V)
  }
  V2<-V
  Asa<-matrix(0,nrow=p,ncol=m)
  for (j in 1:m){
    Asa[,j]<-sqrt(lambda[j])*V2[,j]
  } 
  h2<-diag(Asa%*%t(Asa))
  Dsa<-diag(S-h2) 
  return(list(Asa=Asa,Dsa=Dsa))
}
