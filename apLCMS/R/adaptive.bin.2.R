adaptive.bin.2 <-
function(x, tol, ridge.smoother.window=50, baseline.correct)
{
    masses<-x$masses
    labels<-x$labels
    intensi<-x$intensi
    times<-x$times
    
    rm(x)
    
    base.curve<-unique(times)
    base.curve<-base.curve[order(base.curve)]
    base.curve<-cbind(base.curve, base.curve*0)
    
    curr.order<-order(masses)
    intensi<-intensi[curr.order]
    labels<-labels[curr.order]
    masses<-masses[curr.order]
    
    rm(curr.order)
    
    message("m/z tolerance is: ", tol)
    
    l<-length(masses)
    
    curr.bw<-0.5*tol*max(masses)
    all.mass.den<-density(masses, weights=intensi/sum(intensi), bw=curr.bw, n=2^min(15, floor(log2(l))-2))
    all.mass.turns<-find.turn.point(all.mass.den$y)
    all.mass.vlys<-all.mass.den$x[all.mass.turns$vlys]
    breaks<-c(0, unique(round(approx(masses,1:l,xout=all.mass.vlys,rule=2,ties='ordered')$y))[-1])
    
    grps<-masses*0                   # this is which peak group the signal belongs to, not time group
    
    times<-unique(labels)
    times<-times[order(times)]
    curr.label<-1
    time.range<-diff(range(times))
    aver.time.range<-(max(labels)-min(labels))/length(times)
    if(ridge.smoother.window < time.range/10) ridge.smoother.window<-time.range/10
    
    newprof<-matrix(0, nrow=length(masses),ncol=4)
    height.rec<-matrix(0, nrow=length(masses),ncol=8)
    colnames(height.rec)<-c("group.label", "time.points", "maximum.height", "normalized.mz.range", "normalized.mz.sd.weighted", "normalized.mz.range.before.merge", "normalized.mz.sd.weighted.before.merge","RT.peak.location")
    prof.pointer<-1
    height.pointer<-1
    
    for(i in 1:(length(breaks)-1))
    {
        this.labels<-labels[(breaks[i]+1):breaks[i+1]]
        this.masses<-masses[(breaks[i]+1):breaks[i+1]]
        this.intensi<-intensi[(breaks[i]+1):breaks[i+1]]
        
        curr.order<-order(this.labels)
        this.masses<-this.masses[curr.order]
        this.intensi<-this.intensi[curr.order]
        this.labels<-this.labels[curr.order]
        
        this.bw=0.5*tol*median(this.masses)
        mass.den<-density(this.masses, weights=this.intensi/sum(this.intensi), bw=this.bw)
        mass.den$y[mass.den$y < min(this.intensi)/10]<-0
        mass.turns<-find.turn.point(mass.den$y)
        mass.pks<-mass.den$x[mass.turns$pks]
        mass.vlys<-c(-Inf, mass.den$x[mass.turns$vlys], Inf)
        
        
        for(j in 1:length(mass.pks))
        {
            mass.lower<-max(mass.vlys[mass.vlys < mass.pks[j]])
            mass.upper<-min(mass.vlys[mass.vlys > mass.pks[j]])
            
            if(length(mass.pks) == 1) mass.lower<-mass.lower-1
            mass.sel<-which(this.masses > mass.lower & this.masses <= mass.upper)
            
            if(length(mass.sel) > 0)
            {
                that.labels<-this.labels[mass.sel]
                that.masses<-this.masses[mass.sel]
                that.intensi<-this.intensi[mass.sel]
                
                that.masses.mean<-weighted.mean(that.masses, that.intensi)
                mz.range.before.merge<-abs(diff(range(that.masses)))/that.masses.mean
                mz.sd.before.merge<-sqrt(weighted.mean((that.masses-that.masses.mean)^2, that.intensi))/that.masses.mean
                
                that.merged<-merge_seq_3(that.labels, that.masses, that.intensi)
                if(nrow(that.merged)==1)
                {
                    new.merged<-that.merged
                }else{
                    new.merged<-that.merged[order(that.merged[,1]),]
                }
                
                that.labels<-new.merged[,2]
                that.masses<-new.merged[,1]
                that.intensi<-new.merged[,3]
                that.range<-diff(range(that.labels))
                that.RT.peak.loc<-that.labels[which(that.intensi==max(that.intensi))[1]]
                
                if(that.range > 0.5*time.range & length(that.labels) > that.range/aver.time.range*0.6)
                {
                    that.intensi<-rm.ridge(that.labels, that.intensi, bw=max(ridge.smoother.window, that.range/2))
                    
                    that.sel<-which(that.intensi != 0)
                    that.labels<-that.labels[that.sel]
                    that.masses<-that.masses[that.sel]
                    that.intensi<-that.intensi[that.sel]
                }
                
                that.n<-length(that.masses)
                newprof[prof.pointer:(prof.pointer+that.n-1),]<-cbind(that.masses, that.labels, that.intensi, rep(curr.label, that.n))
                prof.pointer<-prof.pointer+that.n
                that.masses.mean<-weighted.mean(that.masses, that.intensi)
                height.rec[height.pointer,]<-c(curr.label,that.n, max(that.intensi), abs(diff(range(that.masses)))/that.masses.mean, sqrt(weighted.mean((that.masses-that.masses.mean)^2, that.intensi))/that.masses.mean, mz.range.before.merge, mz.sd.before.merge, that.RT.peak.loc)
                height.pointer<-height.pointer+1
                curr.label<-curr.label+1
            }
        }
    }
    
    newprof<-newprof[1:(prof.pointer-1),]
    height.rec<-height.rec[1:(height.pointer-1),]
    
    newprof<-newprof[order(newprof[,1],newprof[,2]),]
    
    raw.prof<-new("list")
    raw.prof$height.rec<-height.rec
    raw.prof$masses<-newprof[,1]
    raw.prof$labels<-newprof[,2]
    raw.prof$intensi<-newprof[,3]
    raw.prof$grps<-newprof[,4]
    raw.prof$times<-times
    raw.prof$tol<-tol
    
    return(raw.prof)
}
