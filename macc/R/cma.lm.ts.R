cma.lm.ts <-
function(dat,Sigma.update=FALSE)
{
  N<-length(dat)
  K<-1
  
  A.hat<-rep(NA,N)
  C2.hat=C.tilde=B.tilde=beta=gamma<-A.hat
  
  i<-1
  for(i in 1:N)
  {
    dd<-dat[[i]]
    A.hat[i]<-(t(dd$Z)%*%dd$M)/(t(dd$Z)%*%dd$Z)
    C2.hat[i]<-(t(dd$Z)%*%dd$R)/(t(dd$Z)%*%dd$Z)
    Sigma.B<-t(cbind(dd$M-dd$Z*A.hat[i],dd$R-dd$Z*C2.hat[i]))%*%cbind(dd$M-dd$Z*A.hat[i],dd$R-dd$Z*C2.hat[i])/nrow(dd)
    
    C.tilde[i]<-(t(dd$M)%*%dd$M%*%t(dd$Z)%*%dd$R-t(dd$Z)%*%dd$M%*%t(dd$M)%*%dd$R)/
      (t(dd$Z)%*%dd$Z%*%t(dd$M)%*%dd$M-t(dd$M)%*%dd$Z%*%t(dd$Z)%*%dd$M)
    gamma[i]<-(sqrt(det(Sigma.B))/Sigma.B[1,1])*((t(dd$Z)%*%dd$M)/(t(dd$Z)%*%dd$Z))
    
    B.tilde[i]<-(t(dd$Z)%*%dd$Z%*%t(dd$M)%*%dd$R-t(dd$M)%*%dd$Z%*%t(dd$Z)%*%dd$R)/
      (t(dd$Z)%*%dd$Z%*%t(dd$M)%*%dd$M-t(dd$M)%*%dd$Z%*%t(dd$Z)%*%dd$M)
    beta[i]<-sqrt(det(Sigma.B))/Sigma.B[1,1]
  }
  
  tau<-(sum((beta-mean(beta))*(B.tilde-mean(B.tilde)))-sum((gamma-mean(gamma))*(C.tilde-mean(C.tilde))))/
    ((sum(beta^2)-N*(mean(beta))^2)+(sum(gamma^2)-N*(mean(gamma))^2))
  
  delta.est<-tau/(sqrt(1+tau^2))
  
  Bt<-B.tilde-tau*beta
  Ct<-C.tilde-tau*gamma
  A.est<-mean(A.hat)
  B.est<-mean(B.tilde)-tau*mean(beta)
  C.est<-mean(C.tilde)+tau*mean(gamma)
  b.hat<-c(A.est,B.est,C.est)
  
  Lambda.est<-matrix(0,3,3)
  diag(Lambda.est)<-(t(A.hat-A.est)%*%(A.hat-A.est)+t(Bt-B.est)%*%(Bt-B.est)+t(Ct-C.est)%*%(Ct-C.est))/(3*N)
  
  sigma1=sigma2<-rep(NA,N)
  for(i in 1:N)
  {
    dd<-dat[[i]]
    
    Y<-cbind(dd$M,dd$R)
    X<-cbind(dd$Z,dd$M)
    if(Sigma.update)
    {
      Theta<-matrix(c(A.hat[i],0,Ct[i],Bt[i]),2,2)
      S<-t(Y-X%*%Theta)%*%(Y-X%*%Theta)
      
      sigma1[i]<-(S[1,1]-delta.est*S[1,2]*sqrt(S[1,1]/S[2,2]))/(nrow(dd)*(1-delta.est^2))
      sigma2[i]<-(S[2,2]-delta.est*S[1,2]*sqrt(S[2,2]/S[1,1]))/(nrow(dd)*(1-delta.est^2))
    }else
    {
      re<-cma.uni.delta(dd,delta.est)
      sigma1[i]<-re$Sigma[1,1]
      sigma2[i]<-re$Sigma[2,2]
    }
  }
  ########################################################################
  HL<-cma.lm.h(dat,delta=delta.est,A.i=A.hat,B.i=Bt,C.i=Ct,b=b.hat,Lambda=Lambda.est,Sigma.update=Sigma.update)
  
  AB.p<-b.hat[1]*b.hat[2]
  AB.d<-mean(C2.hat,na.rm=TRUE)-b.hat[3]
  coe.re<-matrix(NA,6,1)
  colnames(coe.re)<-c("Estimate")
  rownames(coe.re)<-c("A","C","B","C2","AB.prod","AB.diff")
  coe.re[,1]<-c(b.hat[1],b.hat[3],b.hat[2],mean(C2.hat,na.rm=TRUE),AB.p,AB.d)
  sigma.hat<-cbind(sigma1,sigma2)
  colnames(sigma.hat)<-c("E1","E2")
  
  re<-list(delta=delta.est,Coefficients=coe.re,Lambda=Lambda.est,Sigma=sigma.hat,HL=HL$h)
  
  return(re)
}
