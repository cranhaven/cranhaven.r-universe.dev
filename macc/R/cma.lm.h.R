cma.lm.h <-
function(dat,delta=0,A.i,B.i,C.i,b,Lambda,Sigma.update=FALSE)
{
  N<-length(dat)
  K<-1
  
  sigma1=sigma2=n<-matrix(NA,N,K)
  h11=h12=h2<-0
  for(i in 1:N)
  {
    dd<-dat[[i]]
    n[i,1]<-nrow(dd)
    
    Y<-cbind(dd$M,dd$R)
    X<-cbind(dd$Z,dd$M)
    if(Sigma.update)
    {
      Theta<-matrix(c(A.i[i],0,C.i[i],B.i[i]),2,2)
      S<-t(Y-X%*%Theta)%*%(Y-X%*%Theta)
      
      sigma1[i]<-(S[1,1]-delta*S[1,2]*sqrt(S[1,1]/S[2,2]))/(n[i,1]*(1-delta^2))
      sigma2[i]<-(S[2,2]-delta*S[1,2]*sqrt(S[2,2]/S[1,1]))/(n[i,1]*(1-delta^2))
    }else
    {
      re<-cma.uni.delta(dd,delta)
      sigma1[i]<-re$Sigma[1,1]
      sigma2[i]<-re$Sigma[2,2]
    }
    
    P<-matrix(c(-delta*sqrt(sigma2[i]/sigma1[i]),0,0,1,1,0),2,3)
    Q<-c(0,delta*sqrt(sigma2[i]/sigma1[i]))
    b.i<-c(A.i[i],B.i[i],C.i[i])
    V<-matrix(c(1,0,0),nrow=1)
    
    h11<-h11-n[i,1]*log(sigma2[i]*(1-delta^2))/2-t(dd$R-X%*%(P%*%b.i+Q))%*%(dd$R-X%*%(P%*%b.i+Q))/(2*sigma2[i]*(1-delta^2))
    h12<-h12-n[i,1]*log(sigma1[i])/2-t(dd$M-dd$Z%*%V%*%b.i)%*%(dd$M-dd$Z%*%V%*%b.i)/(2*sigma1[i])
    h2<-h2-log(det(Lambda))/2-t(b.i-b)%*%solve(Lambda)%*%(b.i-b)/2
  }
  const1<--log(2*pi)*sum(n)
  const2<--log(2*pi)*N*3/2
  
  h<-(h11+h12+h2)[1,1]
  
  re<-data.frame(h1=(const1+h11+h12)[1,1],h2=(const2+h2)[1,1],h=(const1+const2+h))
  
  return(re)
}
