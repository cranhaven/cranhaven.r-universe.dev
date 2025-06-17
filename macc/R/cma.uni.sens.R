cma.uni.sens <-
function(dat,delta=seq(-1,1,by=0.01),conf.level=0.95)
{
  Z<-matrix(dat$Z,ncol=1)
  M<-matrix(dat$M,ncol=1)
  R<-matrix(dat$R,ncol=1)
  
  n<-nrow(Z)
  
  Coe<-NULL
  Sp<-NULL
  dt<-NULL
  for(i in 1:length(delta))
  {
    if(abs(delta[i])!=1)
    {
      dt<-c(dt,delta[i])
      re<-cma.uni.delta(dat,delta=delta[i],conf.level=conf.level)
      table1<-re$Coefficients
      Coe<-rbind(Coe,c(t(table1)))
      colnames(Coe)<-paste(rep(rownames(table1),each=ncol(table1)),
                           rep(colnames(table1),nrow(table1)),sep=".")
      Sp<-rbind(Sp,c(re$Sigma[1,1],re$Sigma[2,2],re$Sigma[1,2]))
      colnames(Sp)<-c("sigma1^2","sigma2^2","rho")
    }  
  }
  return(list(Coefficients=cbind(delta=dt,Coe),Covariance=cbind(Sp,delta=dt)))
}
