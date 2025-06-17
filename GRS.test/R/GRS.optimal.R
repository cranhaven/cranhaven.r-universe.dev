GRS.optimal <-
function(T,N,K,theta,ratio,p=0.5,k=1,Graph=TRUE){
  
  alphavec=seq(0,1,0.00001)[2:100000]
  df1=N; df2=T-N-K
  critvec = qf(alphavec,df1=df1, df2=df2, lower.tail = FALSE)
  
  lamda=(T/ratio^2)/(1+1/theta^2) * (1-ratio^2)
  powervec = pf(critvec,df1=df1, df2=df2, ncp=lamda,lower.tail = FALSE)
  betavec=1-powervec
  
  loss=p*alphavec+(1-p)*k*betavec
  dd=cbind(alphavec,betavec,loss)
  dd2= dd[dd[,3] == min(dd[,3]),];names(dd2)=NULL
  alphas=dd2[1]; betas=dd2[2]
  crits = qf(alphas,df1=df1, df2=df2, lower.tail = FALSE)
  
  if(Graph==TRUE){
  plot(betavec,alphavec,type="l",xlim=c(0,1),col=1,lwd=2,ylab="alpha",xlab="beta",main="Optimal Significance Level: GRS test") 
  points(betas,alphas,col=4,pch=15,cex=1.5)
  abline(h = 0.05, col=2,lwd=2)
  abline(v=seq(0,1,0.1), col="lightgray", lty="dotted")
  abline(h=seq(0,1,0.1), col="lightgray", lty="dotted")}
  return(list(opt.sig=alphas,opt.crit=crits,opt.beta=betas))
}
