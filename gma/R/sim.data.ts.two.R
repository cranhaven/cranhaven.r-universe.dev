sim.data.ts.two <-
function(Z.list,N,theta,Sigma,W,Delta=NULL,p=NULL,Lambda=diag(rep(1,3)),nburn=100)
{
  n<-rep(NA,N)
  for(i in 1:N)
  {
    n[i]<-length(Z.list[[i]])
  }
  
  s.Lambda<-svd(Lambda)
  Lambda.root<-s.Lambda$u%*%diag(sqrt(s.Lambda$d))%*%t(s.Lambda$v)
  
  eta<-matrix(rnorm(3*N),nrow=N)%*%Lambda.root
  A<-theta[1]+eta[,1]
  B<-theta[2]+eta[,2]
  C<-theta[3]+eta[,3]
  
  dat<-list()
  error<-list()
  for(i in 1:N)
  {
    re.tmp<-sim.data.ts.single(n[i],Z.list[[i]],A[i],B[i],C[i],Sigma,W,Delta,p,nburn)
    dat[[i]]<-re.tmp$data
    error[[i]]<-re.tmp$error
  }
  
  re<-list(data=dat,error=error,A=A,B=B,C=C,type="twolevel")
  return(re)
}
