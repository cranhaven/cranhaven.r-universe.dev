cma.uni.sens.ts.arp.error <-
function(dat,delta=seq(-1,1,by=0.01),p=1,conf.level=0.95)
{
  Coe=Sp=dt=Wmat<-NULL
  for(i in 1:length(delta))
  {
    if(abs(delta[i])!=1)
    {
      # delta
      dt<-c(dt,delta[i])
      
      re<-cma.uni.delta.ts.arp.error(dat,delta=delta[i],p=p,conf.level=conf.level)
      
      # Coefficients and confidence intervals
      Coe<-rbind(Coe,c(t(re$Coefficients)))
      colnames(Coe)<-paste(rep(rownames(re$Coefficients),each=ncol(re$Coefficients)),
                           rep(colnames(re$Coefficients),nrow(re$Coefficients)),sep=".")
      
      # Sigma
      Sp<-rbind(Sp,c(re$Sigma[1,1],re$Sigma[2,2],re$Sigma[1,2]))
      colnames(Sp)<-c("sigma12","sigma22","rho")
      
      # W matrix
      Wmat<-rbind(Wmat,c(re$W))
      colnames(Wmat)<-paste0(rep(c("W11","W21","W12","W22"),p),"_",rep(1:p,each=4))
    }
  }
  return(list(coefficients=cbind(delta=dt,Coe),Sigma=cbind(Sp,delta=dt),W=Wmat))
}
