plot_cdf_2d <-
function(rawname, f, mzlim,timelim, lwd=1)
### rawname is the cdf file name
### f is the output object of prof.to.features()
### lwd is line width
{
    ########### read the raw table
    this.raw<-load.lcms(rawname)
    masses<-this.raw$masses
    intensi<-this.raw$intensi
    labels<-this.raw$labels
    times<-this.raw$times
    rm(this.raw)
    
    times<-times[order(times)]
    base.curve<-unique(times)
    base.curve<-base.curve[order(base.curve)]
    base.curve<-cbind(base.curve, base.curve*0)
    
    curr.order<-order(masses)
    intensi<-intensi[curr.order]
    labels<-labels[curr.order]
    masses<-masses[curr.order]
    
    
    #####################
    sel<-which(labels>=timelim[1] & labels<=timelim[2] & masses>=mzlim[1] & masses <=mzlim[2])
    ex.nc<-cbind(labels[sel], masses[sel])
    plot(ex.nc[,1:2],cex=.1)
    
    f<-f[f[,1]>=mzlim[1] & f[,1]<=mzlim[2],]
    for(i in 1:nrow(f))
    {
        this.mz<-f[i,1]
        this.time<-c(f[i,2]-f[i,3], f[i,2]+f[i,3])
        lines(this.time,rep(this.mz,2),col="red",lwd=lwd)
    }
}
