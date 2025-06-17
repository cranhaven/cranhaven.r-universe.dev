GRS.Power <-
function(T,N,K,theta,ratio,alpha=0.05,xmax=10,Graph=TRUE){

x=seq(0,xmax,0.01)
lamda=(T/ratio^2)/(1+1/theta^2) * (1-ratio^2)
df1=N; df2=T-N-K
cr=qf(1-alpha,df1=df1,df2=df2)
power = pf(cr,df1=df1, df2=df2, ncp=lamda,lower.tail = FALSE)

if (Graph==TRUE){
  
density=df(x,df1=df1, df2=df2)
plot(x,density,type="l",lwd=3,main="Density Functions Under H0 and H1")
density1=df(x,df1=df1, df2=df2,ncp=lamda)
points(x,density1,type="l",col="red",lwd=3)


region.x=x[ x > cr]
region.y=density[ x > cr]
region.x=c(cr,region.x,tail(region.x,1))
region.y=c(0,region.y,0)
polygon(region.x,region.y,density=-1,col="gray")

region.x=x[ x > cr]
region.y=density1[ x > cr]
region.x=c(cr,region.x,tail(region.x,1))
region.y=c(0,region.y,0)
polygon(region.x,region.y,density=10,col="red")
abline(v=cr,col="blue",lwd=3)
abline(h=0,lwd=1)}

return(list(Power=power,Critical.value=cr)) }
