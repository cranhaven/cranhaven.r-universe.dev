cma.uni.ts.arp.error.ll <-
function(dat,Theta,W,Sigma,p=NULL)
{
  Z<-matrix(dat$Z,ncol=1)
  M<-matrix(dat$M,ncol=1)
  R<-matrix(dat$R,ncol=1)
  
  n<-nrow(Z)
  
  if(is.null(p))
  {
    p<-nrow(W)/2
  }
  
  # time series: AR(p) autoregressive errors
  Zx<-apply(matrix(1:p,ncol=1),1,function(x){return(Z[(p+1-x):(n-x)])})
  Mx<-apply(matrix(1:p,ncol=1),1,function(x){return(M[(p+1-x):(n-x)])})
  Rx<-apply(matrix(1:p,ncol=1),1,function(x){return(R[(p+1-x):(n-x)])})
  
  Zy<-matrix(Z[-(1:p),1],ncol=1)
  My<-matrix(M[-(1:p),1],ncol=1)
  Ry<-matrix(R[-(1:p),1],ncol=1)
  
  X<-cbind(Zy,Zx,Mx,Rx)
  
  phi<-matrix(NA,p,2)
  colnames(phi)<-c("phi1","phi2")
  phi[,1]<--W[seq(1,2*p-1,by=2),1]*Theta[1,1]-W[seq(2,2*p,by=2),1]*Theta[1,2]
  phi[,2]<--W[seq(1,2*p-1,by=2),2]*Theta[1,1]-W[seq(2,2*p,by=2),2]*Theta[1,2]
  
  Psi<-matrix(NA,2*p,2)
  Psi[seq(1,2*p-1,by=2),1]<-W[seq(1,2*p-1,by=2),1]-W[seq(2,2*p,by=2),1]*Theta[2,2]
  Psi[seq(2,2*p,by=2),1]<-W[seq(2,2*p,by=2),1]
  Psi[seq(1,2*p-1,by=2),2]<-W[seq(1,2*p-1,by=2),2]-W[seq(2,2*p,by=2),2]*Theta[2,2]
  Psi[seq(2,2*p,by=2),2]<-W[seq(2,2*p,by=2),2]
  
  theta1<-c(Theta[1,1],phi[,1],Psi[seq(1,2*p-1,by=2),1],Psi[seq(2,2*p,by=2),1])
  theta2<-c(Theta[1,2],phi[,2],Psi[seq(1,2*p-1,by=2),2],Psi[seq(2,2*p,by=2),2])
  
  sigma12<-Sigma[1,1]
  sigma22<-Sigma[2,2]
  delta<-Sigma[1,2]/sqrt(sigma12*sigma22)
  
  kappa<-delta*sqrt(sigma22/sigma12)
  
  const<--(n-p)*log(sigma12*sigma22*(1-delta^2))/2
  
  ll1<-(-t(My-X%*%theta1)%*%(My-X%*%theta1)/(2*sigma12))[1,1]
  e2<-(Ry-My*Theta[2,2]-X%*%theta2)-kappa*(My-X%*%theta1)
  ll2<-(-t(e2)%*%e2/(2*sigma22*(1-delta^2)))[1,1]
  
  ll<-const+ll1+ll2
  return(ll)
}
