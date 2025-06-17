sim.data.ts.single <-
function(n,Z,A,B,C,Sigma,W,Delta=NULL,p=NULL,nburn=100)
{
  if(is.null(Delta)==TRUE)
  {
    Delta<-Sigma
  }
  
  if(is.null(p)==TRUE)
  {
    p<-nrow(W)/2
  }
  
  # n subjects and nburn burning samples
  nt<-n+nburn
  
  # For AR(p) model, need to generate p points first
  Ep<-matrix(NA,p,2)
  colnames(Ep)<-c("E1","E2")
  # initial error
  s0<-svd(Delta)
  u<-rnorm(2)
  Ep[1,]<-s0$u%*%diag(sqrt(s0$d))%*%t(s0$v)%*%u
  s<-svd(Sigma)
  
  if(p>1)
  {
    for(j in 2:p)
    {
      u<-rnorm(2)
      e<-s$u%*%diag(sqrt(s$d))%*%t(s$v)%*%u
      
      Ep[j,]<-t(W[1:(2*(j-1)),])%*%c(t(Ep[(j-1):1,]))+e
    } 
  }
  
  E<-matrix(0,nt+p,2)
  E[1:p,]<-Ep
  for(j in (p+1):nrow(E))
  {
    u<-rnorm(2)
    e<-s$u%*%diag(sqrt(s$d))%*%t(s$v)%*%u
    
    E[j,]<-t(W)%*%c(t(E[(j-1):(j-p),]))+e
  }
  
  E1<-E[(nburn+p+1):(nrow(E)),1]
  E2<-E[(nburn+p+1):(nrow(E)),2]
  
  M<-Z*A+E1
  R<-Z*C+M*B+E2
  
  re1<-data.frame(Z=Z,M=M,R=R)
  re2<-data.frame(E1=E1,E2=E2)
  re<-list(data=re1,error=re2)
  return(re)
}
