plot_txt_2d <-
function(rawname, f, mzlim,timelim, lwd=1)
### rawname is the text file name
### f is the output object of prof.to.features()
### lwd is line width
{
    ########### read the raw table
    ex.nc<-read.table(rawname, header=T, sep="\t")
    ex.nc<-as.matrix(ex.nc)
    ex.nc<-ex.nc[ex.nc[,3] != 0,]
    
    #####################
    
    ex.nc<-ex.nc[ex.nc[,1]>=timelim[1] & ex.nc[,1]<=timelim[2] & ex.nc[,2]>=mzlim[1] & ex.nc[,2] <=mzlim[2],]
    plot(ex.nc[,1:2],cex=.1)
    
    f<-f[f[,1]>=mzlim[1] & f[,1]<=mzlim[2],]
    for(i in 1:nrow(f))
    {
        this.mz<-f[i,1]
        this.time<-c(f[i,2]-f[i,3], f[i,2]+f[i,3])
        lines(this.time,rep(this.mz,2),col="red", lwd=lwd)
    }
}
