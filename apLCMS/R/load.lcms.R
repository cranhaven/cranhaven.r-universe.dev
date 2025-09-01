load.lcms <-
function(filename)
{
    splitname<-strsplit(filename,"\\.")[[1]]
    if(tolower(splitname[length(splitname)]) == "cdf")
    {
        mz.conn<-openMSfile(filename,backend="netCDF")
    }else{
        mz.conn<-openMSfile(filename)
    }
    
    masses<-NULL
    intensi<-NULL
    labels<-NULL
    b<-header(mz.conn)$retentionTime
    
    segs<-seq(0, length(b), by=200)
    if((length(b) %% 200) != 0) segs<-c(segs, length(b))
    
    for(n in 2:length(segs))
    {
        a<-peaks(mz.conn, scans=(segs[n-1]+1):segs[n])
        
        this.masses<-NULL
        this.intensi<-NULL
        this.labels<-NULL
        
        for(i in 1:length(a))
        {
            this.a<-a[[i]]
            if(!is.null(nrow(this.a)))
            {
                this.a<-this.a[this.a[,2]>1e-10,]
                if(is.null(nrow(this.a))) this.a<-matrix(this.a, nrow=1)
                
                this.masses<-c(this.masses, this.a[,1])
                this.intensi<-c(this.intensi, this.a[,2])
                this.labels<-c(this.labels, rep(b[segs[n-1]+i],nrow(this.a)))
            }else{
                b[segs[n-1]+i]<-NA
            }
        }
        
        masses<-c(masses, this.masses)
        intensi<-c(intensi, this.intensi)
        labels<-c(labels, this.labels)
    }
    times<-b[!is.na(b)]
    close(mz.conn)
    
    return(list(masses=masses, labels=labels, intensi=intensi, times=times))
}
