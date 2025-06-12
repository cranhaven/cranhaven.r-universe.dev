xlxmx<-function(X,s)
{
	x=X[,1]
	n=dim(X)[2]-1
	xmax=dim(X)[1]
	lx=mx=rep(0,xmax)
	for(i in 1:xmax) mx[i]=sum(X[i,2:(n+1)])/n
	mx=s*mx
	Y=X*0+1
	for(j in 2:(n+1)) 
	{ 
	i=xmax
	while(X[i,j]==0)
		{
		Y[i,j]=0
		i=i-1
		}

	}
for(i in 1:xmax) lx[i]=sum(Y[i,2:(n+1)])/n
data.frame(cbind(x,lx,mx))
}
