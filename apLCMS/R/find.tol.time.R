find.tol.time <-
function(mz, chr, lab, num.exp, mz.tol=2e-5, chr.tol=NA, aver.bin.size=200, min.bins=50, max.bins=100, max.mz.diff=0.01, max.num.segments=10000)
{
    o<-order(mz)
    mz<-mz[o]
    chr<-chr[o]
    lab<-lab[o]
    rm(o)
    
    l<-length(mz)
    
    breaks<-c(0, which((mz[2:l]-mz[1:(l-1)]) > min(max.mz.diff, mz.tol*((mz[2:l]+mz[1:(l-1)])/2))), l)
    
    for(i in 2:length(breaks))
    {
        this.o<-order(chr[(breaks[i-1]+1):breaks[i]])
        this.o<-this.o+breaks[i-1]
        mz[(breaks[i-1]+1):breaks[i]]<-mz[this.o]
        chr[(breaks[i-1]+1):breaks[i]]<-chr[this.o]
        lab[(breaks[i-1]+1):breaks[i]]<-lab[this.o]
    }
    
    breaks<-breaks[c(-1,-length(breaks))]
    if(is.na(chr.tol))
    {
        da<-0
        if(length(breaks)>max.num.segments)
        {
            s<-floor(seq(2, length(breaks), length.out=max.num.segments))
        }else{
            s<-2:length(breaks)
        }
        
        for(i in s)
        {
            this.sel<-(breaks[i-1]+1):breaks[i]
            
            if(length(this.sel) <= 3*num.exp)
            {
                this.d<-as.vector(dist(chr[this.sel]))
                if(length(this.d)>100) this.d<-sample(this.d,100)
                da<-c(da,this.d)
            }
        }
        
        da<-da[!is.na(da)]
        uppermost<-max(da)
        n=min(max.bins,max(min.bins, round(length(da)/aver.bin.size)))
        des<-density(da,kernel="gaussian",n=n, bw=uppermost/n*2,from=0)
        y<-des$y[des$x>0]
        x<-des$x[des$x>0]
        
        this.l<-lm(y[x>uppermost/4]~x[x>uppermost/4])
        
        exp.y<-this.l$coef[1]+this.l$coef[2]*x
        
        plot(x,y,main="find retention time tolerance", xlab="Delta", ylab="Density",cex=0.25)
        lines(x,exp.y,col="red")
        y2<-y[1:(length(y)-1)]
        y3<-y[2:(length(y))]
        y2[which(y2<y3)]<-y3[which(y2<y3)]
        y[1:(length(y)-1)]<-y2
        
        yy<-cumsum(y>1.5*exp.y)
        yi<-1:length(yy)
        sel<-min(which(yy<yi))-1
        
        abline(v=x[sel],col="blue")
        chr.tol<-x[sel]
    }
    
    da<-chr[2:l]-chr[1:(l-1)]
    breaks.2<-which(da>chr.tol)
    all.breaks<-c(0, unique(c(breaks, breaks.2)), l)
    all.breaks<-all.breaks[order(all.breaks)]
    
    grps<-1:length(mz)
    for(i in 2:length(all.breaks))
    {
        grps[(all.breaks[i-1]+1):all.breaks[i]]<-i
    }
    
    to.return<-list(mz=mz, chr=chr, lab=lab, grps=grps, chr.tol=chr.tol, mz.tol=mz.tol)
    return(to.return)
}
