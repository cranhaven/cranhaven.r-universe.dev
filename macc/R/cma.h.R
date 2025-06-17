cma.h <-
function(dat,delta=0,A.ik,B.ik,C.ik,b,u,Phi,Lambda,random.indep=TRUE,u.int=FALSE,Sigma.update=FALSE)
{
  N<-length(dat)
  K<-length(dat[[1]])
  
  sigma1=sigma2=n<-matrix(NA,N,K)
  h11=h12=h13=h14=h2=h3<-0
  for(i in 1:N)
  {
    for(j in 1:K)
    {
      dd<-dat[[i]][[j]]
      n[i,j]<-nrow(dd)
      if(Sigma.update)
      {
        Y<-cbind(dd$M,dd$R)
        X<-cbind(dd$Z,dd$M)
        Theta<-matrix(c(A.ik[i,j],0,C.ik[i,j],B.ik[i,j]),2,2)
        S<-t(Y-X%*%Theta)%*%(Y-X%*%Theta)
        
        sigma1[i,j]<-(S[1,1]-delta*S[1,2]*sqrt(S[1,1]/S[2,2]))/(nrow(dd)*(1-delta^2))
        sigma2[i,j]<-(S[2,2]-delta*S[1,2]*sqrt(S[2,2]/S[1,1]))/(nrow(dd)*(1-delta^2))
      }else
      {
        re<-cma.uni.delta(dd,delta)
        sigma1[i,j]<-re$Sigma[1,1]
        sigma2[i,j]<-re$Sigma[2,2]
      }
      
      P<-matrix(c(-delta*sqrt(sigma2[i,j]/sigma1[i,j]),0,0,1,1,0),nrow=2,ncol=3)
      Q<-c(0,delta*sqrt(sigma2[i,j]/sigma1[i,j]))
      X<-cbind(dd$Z,dd$M)
      b.ik<-c(A.ik[i,j],B.ik[i,j],C.ik[i,j])
      V<-matrix(c(1,0,0),nrow=1)
      h11<-h11-(log(sigma2[i,j]*(1-delta^2))*nrow(dd)/2)
      h12<-h12-(t(dd$R-X%*%P%*%b.ik-X%*%Q)%*%(dd$R-X%*%P%*%b.ik-X%*%Q)/(2*sigma2[i,j]*(1-delta^2)))[1,1]
      h13<-h13-log(sigma1[i,j])*nrow(dd)/2
      h14<-h14-(t(dd$M-dd$Z%*%V%*%b.ik)%*%(dd$M-dd$Z%*%V%*%b.ik)/(2*sigma1[i,j]))[1,1]
      h2<-h2-log(det(Lambda))/2-(t(b.ik-b-u[i,])%*%solve(Lambda)%*%(b.ik-b-u[i,])/2)[1,1]
    }
    h3<-h3-log(det(Phi))/2-(t(u[i,])%*%solve(Phi)%*%u[i,]/2)[1,1]
  }
  const1<--log(2*pi)*sum(n)
  const2<--log(2*pi)*N*K*3/2
  const3<--log(2*pi)*N*3/2
  
  if(random.indep&u.int)
  {
    l.A=l.B=l.C<-0
    V.A<-diag(rep(Lambda[1,1],K,K))+Phi[1,1]*matrix(1,K,K)
    V.B<-diag(rep(Lambda[2,2],K,K))+Phi[2,2]*matrix(1,K,K)
    V.C<-diag(rep(Lambda[3,3],K,K))+Phi[3,3]*matrix(1,K,K)
    for(i in 1:N)
    {
      l.A<-l.A+(-log(det(V.A))/2-t(A.ik[i,]-b[1])%*%solve(V.A)%*%(A.ik[i,]-b[1]))[1,1]
      l.B<-l.B+(-log(det(V.B))/2-t(B.ik[i,]-b[2])%*%solve(V.B)%*%(B.ik[i,]-b[2]))[1,1]
      l.C<-l.C+(-log(det(V.C))/2-t(C.ik[i,]-b[3])%*%solve(V.C)%*%(C.ik[i,]-b[3]))[1,1]
    }
    h2<-l.A+l.B+l.C
    h<-h11+h12+h13+h14+h2
    re<-data.frame(h1=(const1+h11+h12+h13+h14),h2=const2+h2,h=const1+const2+h)
  }else
  {
    if(!random.indep&u.int)
    {
      warning("Independence in random effects assumption is enforced when calculating the likelihood.")
    }
    h<-h11+h12+h13+h14+h2+h3
    re<-data.frame(h1=(const1+h11+h12+h13+h14),h2=const2+h2,h3=const3+h3,h=const1+const2+const3+h)
  }
  return(re)
}
