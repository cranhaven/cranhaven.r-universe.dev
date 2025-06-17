cma.ts.arp.error.lm.h <-
function(dat,delta=0,Ai,Bi,Ci,Wi,b,Lambda,p=NULL,Sigma.update=FALSE)
{
  N<-length(dat)
  K<-1
  
  if(is.null(p))
  {
    p<-dim(Wi)[2]/2
  }
  
  sigma1.hat=sigma2.hat=n<-matrix(NA,N,K)
  h1=h2<-0
  for(i in 1:N)
  {
    dd<-dat[[i]]
    n[i,1]<-nrow(dd)
    
    ###################################################
    # h1
    Zx<-apply(matrix(1:p,ncol=1),1,function(x){return(dd$Z[(p+1-x):(n[i,1]-x)])})
    Mx<-apply(matrix(1:p,ncol=1),1,function(x){return(dd$M[(p+1-x):(n[i,1]-x)])})
    Rx<-apply(matrix(1:p,ncol=1),1,function(x){return(dd$R[(p+1-x):(n[i,1]-x)])})
    
    Zy<-matrix(dd$Z[-(1:p)],ncol=1)
    My<-matrix(dd$M[-(1:p)],ncol=1)
    Ry<-matrix(dd$R[-(1:p)],ncol=1)
    
    X<-cbind(Zy,Zx,Mx,Rx)
    
    Theta<-matrix(c(Ai[i],0,Ci[i],Bi[i]),2,2)
    
    phi<-matrix(NA,p,2)
    colnames(phi)<-c("phi1","phi2")
    phi[,1]<--Wi[i,seq(1,2*p-1,by=2),1]*Theta[1,1]-Wi[i,seq(2,2*p,by=2),1]*Theta[1,2]
    phi[,2]<--Wi[i,seq(1,2*p-1,by=2),2]*Theta[1,1]-Wi[i,seq(2,2*p,by=2),2]*Theta[1,2]
    
    Psi<-matrix(NA,2*p,2)
    Psi[seq(1,2*p-1,by=2),1]<-Wi[i,seq(1,2*p-1,by=2),1]-Wi[i,seq(2,2*p,by=2),1]*Theta[2,2]
    Psi[seq(2,2*p,by=2),1]<-Wi[i,seq(2,2*p,by=2),1]
    Psi[seq(1,2*p-1,by=2),2]<-Wi[i,seq(1,2*p-1,by=2),2]-Wi[i,seq(2,2*p,by=2),2]*Theta[2,2]
    Psi[seq(2,2*p,by=2),2]<-Wi[i,seq(2,2*p,by=2),2]
    
    theta1<-c(Theta[1,1],phi[,1],Psi[seq(1,2*p-1,by=2),1],Psi[seq(2,2*p,by=2),1])
    theta2<-c(Theta[1,2],phi[,2],Psi[seq(1,2*p-1,by=2),2],Psi[seq(2,2*p,by=2),2])
    
    if(Sigma.update)
    {
      e1<-My-X%*%theta1
      e2<-Ry-My*Bi[i]-X%*%theta2
      
      S<-matrix(NA,2,2)
      S[1,1]<-t(e1)%*%e1
      S[1,2]=S[2,1]<-t(e1)%*%e2
      S[2,2]<-t(e2)%*%e2
      
      sigma1.hat[i,1]<-(S[1,1]-delta*S[1,2]*sqrt(S[1,1]/S[2,2]))/((n[i,1]-p)*(1-delta^2))
      sigma2.hat[i,1]<-(S[2,2]-delta*S[1,2]*sqrt(S[2,2]/S[1,1]))/((n[i,1]-p)*(1-delta^2))
    }else
    {
      re<-cma.uni.delta.ts.arp.error(dd,delta,p=p,var.asmp=FALSE)
      sigma1.hat[i,1]<-re$Sigma[1,1]
      sigma2.hat[i,1]<-re$Sigma[2,2]
    }
    
    Sigma<-matrix(c(sigma1.hat[i,1],delta*sqrt(sigma1.hat[i,1]*sigma2.hat[i,1]),
                    delta*sqrt(sigma1.hat[i,1]*sigma2.hat[i,1]),sigma2.hat[i,1]),2,2)
    
    h1<-h1+cma.uni.ts.arp.error.ll(dd,Theta,W=matrix(Wi[i,,],ncol=2),Sigma=Sigma,p=p)
    
    b.hat<-c(Ai[i],Bi[i],Ci[i])
    
    h2<-h2-((log(det(Lambda))+t(b.hat-b)%*%solve(Lambda)%*%(b.hat-b))/2)[1,1]
  }
  
  const1<--log(2*pi)*sum(n-p)
  const2<--log(2*pi)*N*3/2
  
  h<-h1+h2
  re<-data.frame(h1=const1+h1,h2=const2+h2,h=const1+const2+h)
  
  return(re)
}
