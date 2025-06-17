GRS.optimalweight <-
function(T,N,K,theta,ratio,delta=3,p=0.5,k=1,Graph=TRUE){

lamda=(T/ratio^2)/(1+1/theta^2) * (1-ratio^2)
alphavec=seq(0,1,0.00001)[2:100000]
df1=N; df2=T-N-K
critvec = qf(alphavec,df1=df1, df2=df2, lower.tail = FALSE)

D=Folded.Normal(m=lamda,s=delta,Graph)
ff1=D$x; w=D$w
stat=numeric()
for(i in 1:length(ff1)){
  powervec = pf(critvec,df1=df1, df2=df2, ncp=ff1[i],lower.tail = FALSE)
  betavec=1-powervec
  
  loss=p*alphavec+(1-p)*k*betavec
  dd=cbind(alphavec,betavec,loss)
  dd2= dd[dd[,3] == min(dd[,3]),];names(dd2)=NULL
  alphas=dd2[1]; betas=dd2[2]
  stat=c(stat,alphas)}
  alphas=sum(w*stat); crits=qf(alphas,df1=df1, df2=df2, lower.tail = FALSE)
  return(list(opt.sig=alphas,opt.crit=crits))}
