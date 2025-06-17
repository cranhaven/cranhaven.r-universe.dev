GRS.Powerfunc <-
function(Tvec,N,K,theta,alpha=0.05){
ratiovec=seq(0.01,1,0.01)
powermat=matrix(NA,nrow=length(Tvec),ncol=length(ratiovec))
for (i1 in 1:length(Tvec)){
  T = Tvec[i1]  
  for (i2 in 1:length(ratiovec)){
  ratio = ratiovec[i2]
  df1=N; df2=T-N-K
  crit = qf(alpha,df1=df1, df2=df2, lower.tail = FALSE)
  lamda=(T/ratio^2)/(1+1/theta^2) * (1-ratio^2)
  power = pf(crit,df1=df1, df2=df2, ncp=lamda,lower.tail = FALSE)
  powermat[i1,i2]=power }
}

plot(ratiovec,powermat[1,],xlim=c(0,1),ylim=c(0,1),type="l",col=1,lwd=2,ylab="power",main="Power Function: GRS Test",xlab="ratio")
if (length(Tvec) > 1)
{for (i in 2:length(Tvec)) points(ratiovec,powermat[i,],xlim=c(0,1),ylim=c(0,1),type="l",col=i,lwd=2) }
abline(v=seq(0,1,0.1), col="lightgray", lty="dotted")
abline(h=seq(0,1,0.1), col="lightgray", lty="dotted")
tem=as.character()
for(i in 1:length(Tvec)) tem=c(tem, paste("T",Tvec[i],sep="=")) 
rownames(powermat)=tem
return(Power=t(powermat))}
