present.cdf.3d <-
function(prof, fill.holes=TRUE, transform="none", time.lim=NA, mz.lim=NA, box = TRUE, axes = TRUE)
{
    fillholes<-function(times, b)
    {
        b<-b[order(b[,1]),]
        newb<-times[times>=min(b[,1]) & times<=max(b[,1])]
        newb<-cbind(newb, rep(NA,length(newb)))
        newb[newb[,1] %in% b[,1],2]<-b[,2]
        exist.b<-which(!is.na(newb[,2]))
        for(k in 1:(length(exist.b)-1))
        {
            if(exist.b[k+1]-exist.b[k] > 1)
            {
                for(m in (exist.b[k]+1):(exist.b[k+1]-1))
                {
                    newb[m,2]<-(newb[exist.b[k+1],2]-newb[exist.b[k],2])/(newb[exist.b[k+1],1]-newb[exist.b[k],1])*(newb[m,1]-newb[exist.b[k],1])+ newb[exist.b[k],2]
                }
            }
        }
        return(newb)
    }
    r3dDefaults <- rgl::par3d("defaults")
    r3dDefaults$windowRect <- c(0,50, 700, 700)
    a<-prof
    
    
    if(!is.na(time.lim[1]))
    {
        a<-a[a[,2] >= time.lim[1] & a[,2] <= time.lim[2],]
    }
    if(!is.na(mz.lim[1]))
    {
        a<-a[a[,1] >= mz.lim[1] & a[,1] <= mz.lim[2],]
    }
    
    
    a<-a[order(a[,4]),]
    n<-nrow(a)
    breaks<-c(0,which(a[1:(n-1),4] != a[2:n,4]),n)
    for(i in 1:(length(breaks)-1))
    {
        a[(breaks[i]+1):breaks[i+1],1]<-median(a[(breaks[i]+1):breaks[i+1],1])
    }
    
    if(transform=="sqrt") a[,3]<-sqrt(a[,3])
    if(transform=="cuberoot") a[,3]<-a[,3]^(1/3)
    
    times<-unique(c(time.lim,a[,2]))
    times<-times[order(times, na.last=FALSE)]
    if(!is.na(time.lim[1]))
    {
        if(min(times)>time.lim[1]) times<-c(time.lim[1], min(times)-0.001, times)
        if(max(times)<time.lim[2]) times<-c(times, max(times)+0.001, time.lim[2])
    }
    times<-times[!is.na(times)]
    masses<-unique(a[,1])
    masses<-masses[order(masses)]
    
    if(fill.holes)
    {
        new.a<-a[1,]
        for(i in 1:length(masses))
        {
            this<-a[a[,1] == masses[i],]
            if(!is.null(dim(this)))
            {
                this.labels<-unique(this[,4])
                for(j in 1:length(this.labels))
                {
                    b<-this[this[,4]==this.labels[j],2:3]
                    if(!is.null(dim(b)))
                    {
                        this.new<-fillholes(times, b)
                        this.new<-cbind(rep(this[1,1],nrow(this.new)), this.new, rep(this.labels[j],nrow(this.new)))
                        new.a<-rbind(new.a, this.new)
                    }
                }
            }
        }
        a<-new.a[-1,]
    }
    
    masses<-unique(c(mz.lim, masses))
    masses<-masses[order(masses)]
    masses<-masses[!is.na(masses)]
    masses<-c(masses,masses-(1e-6), masses+(1e-10),masses+(1e-10+1e-6))
    masses<-masses[order(masses)]
    
    #if(!is.na(mz.lim[1]))
    #{
    #   if(min(masses)>mz.lim[1]) masses<-c(mz.lim[1], (min(masses)+mz.lim[1])/2, masses)
    #   if(max(masses)<mz.lim[2]) masses<-c(masses, (max(masses)+mz.lim[2])/2, mz.lim[2])
    #}
    
    z<-matrix(0,nrow=length(times),ncol=length(masses))
    
    a<-a[order(a[,1]),]
    a[,1]<-as.numeric(as.factor(rank(a[,1])))
    
    a<-a[order(a[,2]),]
    a[,2]<-as.numeric(as.factor(rank(a[,2])))
    
    a<-a[order(a[,1]),]
    a[,1]<-4*(a[,1]-1)+2
    
    for(i in 1:nrow(a))
    {
        b<-a[i,]
        z[b[2],b[1]]<-z[b[2],b[1]+1]<-b[3]
    }
    
    zlim <- range(z)
    
    colorlut <- topo.colors(100)
    #col <- colorlut[ round(log(z+1)/log(zlim[2]+1) * 99)+1 ]
    col<-colorlut[round(sqrt(z)/max(sqrt(z))*99)+1]
    
    #z<-z/max(z)*(max(masses)-min(masses))
    surface3d(times,masses,z,color=col)
    if(is.na(time.lim[1])) time.lim<-range(times)
    if(is.na(mz.lim[1])) mz.lim<-range(masses)
    decorate3d(aspect="iso",xlab="time",ylab="mz",zlab="",xlim=time.lim, ylim=mz.lim, box=box, axes=axes)
    aspect3d(1, 1, 1)
    par3d(userMatrix = rotationMatrix(90*pi/180, -2,1,1.5))
}
