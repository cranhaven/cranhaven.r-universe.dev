plot.LMsearch<-function(x,...){
  object <- x
 # par(mar=c(5, 4, 4, 2) + 0.1,mfrow=c(1,1))
  par(mfrow=c(1,1))
  ylim1 =  min(c(object$Aic,object$Bic))
  ylim2 = max(c(object$Aic,object$Bic))*1.05
  kv <-as.numeric(names(object$Bic))
  matplot(kv,cbind(object$Bic,object$Aic),type="b",pch=1:2,lty=1,col=c(2,4),xaxt="n",xlab="Number of states",ylab="",ylim = c(ylim1,ylim2))
  axis(side=1,at=kv)
  legend("topright",legend=c("BIC","AIC"),col=c(2,4),lty=1,bty="n")
}
