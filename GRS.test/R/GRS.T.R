GRS.T <-
function(N,K,theta,ratio,alpha,beta,Tmax=10000) 
{
  T=seq(N+K+1,Tmax,1)
  lamda=(T/ratio^2)/(1+1/theta^2) * (1-ratio^2)
  df1=N; df2=T-N-K
  
  cr=qf(1-alpha,df1=df1,df2=df2)
  xs = qf(beta,df1=df1, df2=df2, ncp=lamda,lower.tail = TRUE)
  diff=abs(cr-xs)
  index=which.min(diff)
  Ts=T[index]
  
  df1=N; df2=Ts-N-K
  lamda=(Ts/ratio^2)/(1+1/theta^2) * (1-ratio^2)
  cr1=qf(1-alpha,df1=df1,df2=df2)
  return(list(Required.T=Ts,Critical.value=cr1))
}
