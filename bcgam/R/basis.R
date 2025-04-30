#' @keywords internal
monincr=function(x, t)
{
	n=length(x)
	k=length(t)-2
	m=k+2
	sigma=matrix(1:m*n,nrow=m,ncol=n)
	for(j in 1:(k-1)){
	 	i1=x<=t[j]
	 	sigma[j,i1] = 0
	 	i2=x>t[j]&x<=t[j+1]
		sigma[j,i2] = (x[i2]-t[j])^2 / (t[j+2]-t[j]) / (t[j+1]-t[j])
	    i3=x>t[j+1]&x<=t[j+2]
		sigma[j,i3] = 1-(x[i3]-t[j+2])^2/(t[j+2]-t[j+1])/(t[j+2]-t[j])
	    i4=x>t[j+2]
		sigma[j,i4]=1
	}
	
	i1=x<=t[k]
	sigma[k,i1] = 0
	i2=x>t[k]&x<=t[k+1]
	sigma[k,i2] = (x[i2]-t[k])^2 / (t[k+2]-t[k]) / (t[k+1]-t[k])
	i3=x>t[k+1]&x<=t[k+2]
	sigma[k,i3] = 1- (x[i3]-t[k+2])^2/(t[k+2]-t[k+1])/(t[k+2]-t[k])
	i4=x>t[k+2]
	sigma[k,i4]=1
	
	i1=x<=t[2]
	sigma[k+1,i1]=1-(t[2]-x[i1])^2/(t[2]-t[1])^2
	i2=x>t[2]
	sigma[k+1,i2]=1
	
	i1=x<=t[k+1]
	sigma[k+2,i1]=0
	i2=x>t[k+1]&x<=t[k+2]
	sigma[k+2,i2]=(x[i2]-t[k+1])^2/(t[k+2]-t[k+1])^2
	i3=x>t[k+2]
	sigma[k+2,i3]=1
	
	center.vector=apply(sigma,1,mean)

	list(sigma=sigma, center.vector=center.vector)
}


#' @keywords internal
mondecr=function(x, t)
{
	n=length(x)
	k=length(t)-2
	m=k+2
	sigma=matrix(1:m*n,nrow=m,ncol=n)
	for(j in 1:(k-1)){
	 	i1=x<=t[j]
	 	sigma[j,i1] = 0
	 	i2=x>t[j]&x<=t[j+1]
		sigma[j,i2] = (x[i2]-t[j])^2 / (t[j+2]-t[j]) / (t[j+1]-t[j])
	    i3=x>t[j+1]&x<=t[j+2]
		sigma[j,i3] = 1-(x[i3]-t[j+2])^2/(t[j+2]-t[j+1])/(t[j+2]-t[j])
	    i4=x>t[j+2]
		sigma[j,i4]=1
	}

	i1=x<=t[k]
	sigma[k,i1] = 0
	i2=x>t[k]&x<=t[k+1]
	sigma[k,i2] = (x[i2]-t[k])^2 / (t[k+2]-t[k]) / (t[k+1]-t[k])
	i3=x>t[k+1]&x<=t[k+2]
	sigma[k,i3] = 1- (x[i3]-t[k+2])^2/(t[k+2]-t[k+1])/(t[k+2]-t[k])
	i4=x>t[k+2]
	sigma[k,i4]=1	

	i1=x<=t[2]
	sigma[k+1,i1]=1-(t[2]-x[i1])^2/(t[2]-t[1])^2
	i2=x>t[2]
	sigma[k+1,i2]=1

	i1=x<=t[k+1]
	sigma[k+2,i1]=0
	i2=x>t[k+1]&x<=t[k+2]
	sigma[k+2,i2]=(x[i2]-t[k+1])^2/(t[k+2]-t[k+1])^2
	i3=x>t[k+2]
	sigma[k+2,i3]=1
	
	center.vector=apply(sigma,1,mean)
	
	list(sigma=-sigma, center.vector=-center.vector)
}


#' @keywords internal
convex=function(x, t, pred.new=TRUE)
{
	n=length(x)
	k=length(t)-2
	m=k+2
	sigma=matrix(1:m*n,nrow=m,ncol=n)
	for(j in 1:(k-1)){
		i1=x<=t[j]
		sigma[j,i1] = 0
	 	i2=x>t[j]&x<=t[j+1]
	 	sigma[j,i2] = (x[i2]-t[j])^3 / (t[j+2]-t[j]) / (t[j+1]-t[j])/3
	    i3=x>t[j+1]&x<=t[j+2]
	    sigma[j,i3] = x[i3]-t[j+1]-(x[i3]-t[j+2])^3/(t[j+2]-t[j])/(t[j+2]-t[j+1])/3+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	    i4=x>t[j+2]
	    sigma[j,i4]=(x[i4]-t[j+1])+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	}
	i1=x<=t[k]
	sigma[k,i1] = 0
	i2=x>t[k]&x<=t[k+1]
	sigma[k,i2] = (x[i2]-t[k])^3 / (t[k+2]-t[k]) / (t[k+1]-t[k])/3
	i3=x>t[k+1]
	sigma[k,i3] = x[i3]-t[k+1]-(x[i3]-t[k+2])^3/(t[k+2]-t[k])/(t[k+2]-t[k+1])/3+(t[k+1]-t[k])^2/3/(t[k+2]-t[k])-(t[k+2]-t[k+1])^2/3/(t[k+2]-t[k])
	i1=x<=t[2]
	sigma[k+1,i1]=x[i1]-t[1]+(t[2]-x[i1])^3/(t[2]-t[1])^2/3
	i2=x>t[2]
	sigma[k+1,i2]=x[i2]-t[1]
	i1=x<=t[k+1]
	sigma[k+2,i1]=0
	i2=x>t[k+1]
	sigma[k+2,i2]=(x[i2]-t[k+1])^3/(t[k+2]-t[k+1])^2/3

	v1=1:n*0+1
	v2=x
	x.mat=cbind(v1,v2)
	
	if(pred.new==TRUE){
	list(sigma=sigma,x.mat=x.mat)}

	else{
	if(pred.new==FALSE){
	coef=solve(t(x.mat)%*%x.mat)%*%t(x.mat)%*%t(sigma)
	list(sigma=sigma, x.mat=x.mat, center.vector=coef)}
	}

}


#' @keywords internal
concave=function(x, t, pred.new=TRUE)
{
	n=length(x)
	k=length(t)-2
	m=k+2
	sigma=matrix(1:m*n,nrow=m,ncol=n)
	for(j in 1:(k-1)){
		i1=x<=t[j]
		sigma[j,i1] = 0
	 	i2=x>t[j]&x<=t[j+1]
	 	sigma[j,i2] = (x[i2]-t[j])^3 / (t[j+2]-t[j]) / (t[j+1]-t[j])/3
	    i3=x>t[j+1]&x<=t[j+2]
	    sigma[j,i3] = x[i3]-t[j+1]-(x[i3]-t[j+2])^3/(t[j+2]-t[j])/(t[j+2]-t[j+1])/3+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	    i4=x>t[j+2]
	    sigma[j,i4]=(x[i4]-t[j+1])+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	}
	i1=x<=t[k]
	sigma[k,i1] = 0
	i2=x>t[k]&x<=t[k+1]
	sigma[k,i2] = (x[i2]-t[k])^3 / (t[k+2]-t[k]) / (t[k+1]-t[k])/3
	i3=x>t[k+1]
	sigma[k,i3] = x[i3]-t[k+1]-(x[i3]-t[k+2])^3/(t[k+2]-t[k])/(t[k+2]-t[k+1])/3+(t[k+1]-t[k])^2/3/(t[k+2]-t[k])-(t[k+2]-t[k+1])^2/3/(t[k+2]-t[k])
	i1=x<=t[2]
	sigma[k+1,i1]=x[i1]-t[1]+(t[2]-x[i1])^3/(t[2]-t[1])^2/3
	i2=x>t[2]
	sigma[k+1,i2]=x[i2]-t[1]
	i1=x<=t[k+1]
	sigma[k+2,i1]=0
	i2=x>t[k+1]
	sigma[k+2,i2]=(x[i2]-t[k+1])^3/(t[k+2]-t[k+1])^2/3

	v1=1:n*0+1
	v2=x
	x.mat=cbind(v1,v2)

	sigma=-sigma

	if(pred.new==TRUE){
	list(sigma=sigma,x.mat=x.mat)}

	else{
	if(pred.new==FALSE){
	coef=solve(t(x.mat)%*%x.mat)%*%t(x.mat)%*%t(sigma)
	list(sigma=sigma, x.mat=x.mat, center.vector=coef)}
	}
}


#' @keywords internal
incconvex=function(x,t)
{
	n=length(x)
	k=length(t)-2
	m=k+3
	sigma=matrix(1:m*n,nrow=m,ncol=n)
	for(j in 1:(k-1)){
		i1=x<=t[j]
		sigma[j,i1] = 0
	 	i2=x>t[j]&x<=t[j+1]
	 	sigma[j,i2] = (x[i2]-t[j])^3 / (t[j+2]-t[j]) / (t[j+1]-t[j])/3
	    i3=x>t[j+1]&x<=t[j+2]
	    sigma[j,i3] = x[i3]-t[j+1]-(x[i3]-t[j+2])^3/(t[j+2]-t[j])/(t[j+2]-t[j+1])/3+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	    i4=x>t[j+2]
	    sigma[j,i4]=(x[i4]-t[j+1])+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	}
	i1=x<=t[k]
	sigma[k,i1] = 0
	i2=x>t[k]&x<=t[k+1]
	sigma[k,i2] = (x[i2]-t[k])^3 / (t[k+2]-t[k]) / (t[k+1]-t[k])/3
	i3=x>t[k+1]
	sigma[k,i3] = x[i3]-t[k+1]-(x[i3]-t[k+2])^3/(t[k+2]-t[k])/(t[k+2]-t[k+1])/3+(t[k+1]-t[k])^2/3/(t[k+2]-t[k])-(t[k+2]-t[k+1])^2/3/(t[k+2]-t[k])
	i1=x<=t[2]
	sigma[k+1,i1]=x[i1]-t[1]+(t[2]-x[i1])^3/(t[2]-t[1])^2/3
	i2=x>t[2]
	sigma[k+1,i2]=x[i2]-t[1]
	i1=x<=t[k+1]
	sigma[k+2,i1]=0
	i2=x>t[k+1]
	sigma[k+2,i2]=(x[i2]-t[k+1])^3/(t[k+2]-t[k+1])^2/3
	sigma[k+3,]=x

	center.vector=apply(sigma,1,mean)
	
	list(sigma=sigma, center.vector=center.vector)
}


#' @keywords internal
incconcave=function(x,t)
{
	n=length(x)
	k=length(t)-2
	m=k+3
	sigma=matrix(1:(m*n)*0,nrow=m,ncol=n)
	for(j in 1:k){
	 	i1=x<=t[j]
	    sigma[j,i1] = x[i1]-t[1]
	    i2=x>t[j]&x<=t[j+1]
	 	sigma[j,i2] = t[j]-t[1]+((t[j+1]-t[j])^3-(t[j+1]-x[i2])^3)/3/(t[j+1]-t[j])/(t[j+2]-t[j]) +(x[i2]-t[j])*(t[j+2]-t[j+1])/(t[j+2]-t[j])
	 	i3=x>t[j+1]&x<=t[j+2]
		sigma[j,i3] = t[j]-t[1] + (t[j+1]-t[j])^2/3/(t[j+2]-t[j]) + (t[j+2]-t[j+1])*(t[j+1]-t[j])/(t[j+2]-t[j]) +((t[j+2]-t[j+1])^3-(t[j+2]-x[i3])^3)/3/(t[j+2]-t[j+1])/(t[j+2]-t[j])
		i4=x>=t[j+2]
		sigma[j,i4] = t[j]-t[1] + (t[j+1]-t[j])^2/3/(t[j+2]-t[j]) + (t[j+2]-t[j+1])*(t[j+1]-t[j])/(t[j+2]-t[j]) +(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	}
	i1=x<=t[2]
	sigma[k+1,i1]=-(t[2]-x[i1])^3/3/(t[2]-t[1])^2
	i2=x>t[2]
	sigma[k+1,i2]=0
	i1=x<=t[k+1]
	sigma[k+2,i1]=x[i1]-t[1]
	i2=x>t[k+1]&x<=t[k+2]
	sigma[k+2,i2]=t[k+1]-t[1]+((t[k+2]-t[k+1])^2*(x[i2]-t[k+1])-(x[i2]-t[k+1])^3/3)/(t[k+2]-t[k+1])^2
	i3=x>t[k+2]
	sigma[k+2,i3]=t[k+1]-t[1]+((t[k+2]-t[k+1])^2*(t[k+2]-t[k+1])-(t[k+2]-t[k+1])^3/3)/(t[k+2]-t[k+1])^2
	sigma[k+3,]=x

	center.vector=apply(sigma,1,mean)

	list(sigma=sigma, center.vector=center.vector)
}


#' @keywords internal
decconvex=function(x,t)
{
	n=length(x)
	k=length(t)-2
	m=k+3
	sigma=matrix(1:(m*n)*0,nrow=m,ncol=n)
	for(j in 1:k){
	 	i1=x<=t[j]
	    sigma[j,i1] = x[i1]-t[1]
	    i2=x>t[j]&x<=t[j+1]
	 	sigma[j,i2] = t[j]-t[1]+((t[j+1]-t[j])^3-(t[j+1]-x[i2])^3)/3/(t[j+1]-t[j])/(t[j+2]-t[j]) +(x[i2]-t[j])*(t[j+2]-t[j+1])/(t[j+2]-t[j])
	 	i3=x>t[j+1]&x<=t[j+2]
		sigma[j,i3] = t[j]-t[1] + (t[j+1]-t[j])^2/3/(t[j+2]-t[j]) + (t[j+2]-t[j+1])*(t[j+1]-t[j])/(t[j+2]-t[j]) +((t[j+2]-t[j+1])^3-(t[j+2]-x[i3])^3)/3/(t[j+2]-t[j+1])/(t[j+2]-t[j])
		i4=x>=t[j+2]
		sigma[j,i4] = t[j]-t[1] + (t[j+1]-t[j])^2/3/(t[j+2]-t[j]) + (t[j+2]-t[j+1])*(t[j+1]-t[j])/(t[j+2]-t[j]) +(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	}
	i1=x<=t[2]
	sigma[k+1,i1]=-(t[2]-x[i1])^3/3/(t[2]-t[1])^2
	i2=x>t[2]
	sigma[k+1,i2]=0
	i1=x<=t[k+1]
	sigma[k+2,i1]=x[i1]-t[1]
	i2=x>t[k+1]&x<=t[k+2]
	sigma[k+2,i2]=t[k+1]-t[1]+((t[k+2]-t[k+1])^2*(x[i2]-t[k+1])-(x[i2]-t[k+1])^3/3)/(t[k+2]-t[k+1])^2
	i3=x>t[k+2]
	sigma[k+2,i3]=t[k+1]-t[1]+((t[k+2]-t[k+1])^2*(t[k+2]-t[k+1])-(t[k+2]-t[k+1])^3/3)/(t[k+2]-t[k+1])^2
	sigma[k+3,]=x

	center.vector=apply(sigma,1,mean)

	list(sigma=-sigma, center.vector=-center.vector)
}


#' @keywords internal
decconcave=function(x,t)
{
	n=length(x)
	k=length(t)-2
	m=k+3
	sigma=matrix(1:m*n,nrow=m,ncol=n)
	for(j in 1:(k-1)){
		i1=x<=t[j]
		sigma[j,i1] = 0
	 	i2=x>t[j]&x<=t[j+1]
	 	sigma[j,i2] = (x[i2]-t[j])^3 / (t[j+2]-t[j]) / (t[j+1]-t[j])/3
	    i3=x>t[j+1]&x<=t[j+2]
	    sigma[j,i3] = x[i3]-t[j+1]-(x[i3]-t[j+2])^3/(t[j+2]-t[j])/(t[j+2]-t[j+1])/3+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	    i4=x>t[j+2]
	    sigma[j,i4]=(x[i4]-t[j+1])+(t[j+1]-t[j])^2/3/(t[j+2]-t[j])-(t[j+2]-t[j+1])^2/3/(t[j+2]-t[j])
	}
	i1=x<=t[k]
	sigma[k,i1] = 0
	i2=x>t[k]&x<=t[k+1]
	sigma[k,i2] = (x[i2]-t[k])^3 / (t[k+2]-t[k]) / (t[k+1]-t[k])/3
	i3=x>t[k+1]
	sigma[k,i3] = x[i3]-t[k+1]-(x[i3]-t[k+2])^3/(t[k+2]-t[k])/(t[k+2]-t[k+1])/3+(t[k+1]-t[k])^2/3/(t[k+2]-t[k])-(t[k+2]-t[k+1])^2/3/(t[k+2]-t[k])
	i1=x<=t[2]
	sigma[k+1,i1]=x[i1]-t[1]+(t[2]-x[i1])^3/(t[2]-t[1])^2/3
	i2=x>t[2]
	sigma[k+1,i2]=x[i2]-t[1]
	i1=x<=t[k+1]
	sigma[k+2,i1]=0
	i2=x>t[k+1]
	sigma[k+2,i2]=(x[i2]-t[k+1])^3/(t[k+2]-t[k+1])^2/3
	sigma[k+3,]=x

	center.vector=apply(sigma,1,mean)

	list(sigma=-sigma, center.vector=-center.vector)

}



