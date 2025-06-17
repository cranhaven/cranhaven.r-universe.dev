sim.data.multi <-
function(Z.list,N,K=1,Theta,Sigma,Psi=diag(rep(1,3)),Lambda=diag(rep(1,3)))
{
  n<-matrix(NA,N,K)
  for(i in 1:N)
  {
    for(j in 1:K)
    {
      n[i,j]<-length(Z.list[[i]][[j]])
    }
  }
  
  if(K>1)
  {
    s.Psi<-svd(Psi)
    Psi.root<-s.Psi$u%*%diag(sqrt(s.Psi$d))%*%t(s.Psi$v)
    
    s.Lambda<-svd(Lambda)
    Lambda.root<-s.Lambda$u%*%diag(sqrt(s.Lambda$d))%*%t(s.Lambda$v)
    
    u<-matrix(rnorm(3*N),nrow=N)%*%Psi.root
    alpha<-u[,1]
    beta<-u[,2]
    gamma<-u[,3]
    
    A=B=C<-matrix(NA,N,K)
    epsA=epsB=epsC<-matrix(NA,N,K)
    for(i in 1:N)
    {
      eta<-matrix(rnorm(3*K),nrow=K)%*%Lambda.root
      
      epsA[i,]<-eta[,1]
      epsB[i,]<-eta[,2]
      epsC[i,]<-eta[,3]
    }
    A<-Theta[1,1]+matrix(rep(alpha,K),nrow=N)+epsA
    B<-Theta[2,2]+matrix(rep(beta,K),nrow=N)+epsB
    C<-Theta[1,2]+matrix(rep(gamma,K),nrow=N)+epsC
    
    dat<-list()
    for(i in 1:N)
    {
      dat[[i]]<-list()
      for(j in 1:K)
      {
        dat[[i]][[j]]<-sim.data.single(Z.list[[i]][[j]],Theta=matrix(c(A[i,j],0,C[i,j],B[i,j]),2,2),Sigma)
      }
    }
    
    re<-list(data=dat,A=A,B=B,C=C,type="multilevel")
  }else
  {
    s.Lambda<-svd(Lambda)
    Lambda.root<-s.Lambda$u%*%diag(sqrt(s.Lambda$d))%*%t(s.Lambda$v)
    
    eta<-matrix(rnorm(3*N),nrow=N)%*%Lambda.root
    A<-Theta[1,1]+eta[,1]
    B<-Theta[2,2]+eta[,2]
    C<-Theta[1,2]+eta[,3]
    
    dat<-list()
    for(i in 1:N)
    {
      dat[[i]]<-sim.data.single(Z.list[[i]],Theta=matrix(c(A[i],0,C[i],B[i]),2,2),Sigma)
    }
    
    re<-list(data=dat,A=A,B=B,C=C,type="twolevel")
  }
  
  return(re)
}
