find.tol <-
function(a, uppermost=1e-4, aver.bin.size=4000, min.bins=50, max.bins=200)
{
    a<-a[order(a)]
    l<-length(a)
    da<-(a[2:l]-a[1:(l-1)])/((a[2:l]+a[1:(l-1)])/2)
    da<-da[da < uppermost]
    n<-min(max.bins, max(round(length(da)/aver.bin.size), min.bins))
    des<-density(da,kernel="gaussian",n=n, bw=uppermost/n*2,from=0)
    y<-des$y[des$x>0]
    x<-des$x[des$x>0]
    
    to.use<-da[da>max(da)/4]-max(da)/4
    this.rate<-fitdistr(to.use, "exponential")$estimate
    exp.y<-dexp(x,rate=this.rate)
    exp.y<-exp.y*sum(y[x>max(da)/4])/sum(exp.y[x>max(da)/4])
    
    plot(x,y, xlab="Delta",ylab="Density",main="find m/z tolerance",cex=.25)
    lines(x,exp.y,col="red")
    
    yy<-cumsum(y)
    y2<-cumsum(exp.y)
    yy<-cumsum(y>1.5*exp.y)
    yi<-1:length(yy)
    sel<-min(which(yy<yi))-1
    
    abline(v=x[sel],col="blue")
    return(x[sel])
    
}
