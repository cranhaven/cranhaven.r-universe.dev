cont.index <-
function(newprof, min.pres=0.6, min.run=5)
{
    collapse<-function(a)
    {
        a<-a[order(a[,2]),]
        l<-nrow(a)
        a.breaks<-which(a[2:l,2] != a[1:(l-1),2])
        a.breaks<-c(0,a.breaks,l)
        
        newa<-c(0,0,0,0)
        for(i in 2:length(a.breaks))
        {
            sel<-(a.breaks[i-1]+1):a.breaks[i]
            newa<-rbind(newa, c(median(a[a.breaks[i],1]),a[a.breaks[i],2], sum(a[sel,3]),a[a.breaks[i],4]))
        }
        
        return(newa[-1,])
    }
    
    labels<-newprof[,2]
    times<-unique(labels)
    times<-times[order(times)]
    time.points<-length(times)
    
    for(i in 1:length(times)) labels[which(newprof[,2]==times[i])]<-i  #now labels is the index of time points
    newprof[,2]<-labels
    
    times<-times[order(times)]
    l<-nrow(newprof)
    timeline<-rep(0,time.points)
    i=1
    num.stack<-1
    min.count.run<-min.run*time.points/(max(times)-min(times))
    aver.time.range<-(max(times)-min(times))/time.points
    
    grps<-newprof[,4]
    uniq.grp<-unique(grps)
    curr.label<-1
    
    ttt<-table(grps)
    ttt<-ttt[ttt>=max(min.count.run * min.pres,2)]
    uniq.grp<-as.numeric(names(ttt))
    
    newprof<-newprof[newprof[,4] %in% uniq.grp,]
    newprof<-newprof[order(newprof[,4], newprof[,1]),]
    r.newprof<-nrow(newprof)
    breaks<-c(0, which(newprof[1:(r.newprof-1),4] != newprof[2:r.newprof,4]), r.newprof)
    
    new.rec<-newprof*0
    rec.pointer<-1
    
    height.rec<-mz.pres.rec<-time.range.rec<-rep(0,length(breaks))
    mz.pres.ptr<-1
    
    min.run<-round(min.count.run)
    
    for(m in 2:length(breaks))
    {
        this.prof<-newprof[(breaks[m-1]+1):breaks[m],]
        
        this.prof<-this.prof[order(this.prof[,2]),]
        this.times<-this.prof[,2]
        this.intensi<-this.prof[,3]
        this.mass<-this.prof[,1]
        this.grp<-this.prof[1,4]
        
        this.timeline<-timeline
        this.timeline[this.times]<-1
        
        to.keep<-this.times*0
        
        dens<-ksmooth(seq(-min.run+1,length(this.timeline)+min.run), c(rep(0, min.run), this.timeline, rep(0, min.run)), kernel="box", bandwidth=min.run, x.points=1:length(this.timeline))
        dens<-dens$y
        
        if(max(dens) >= min.pres)
        {
            measured.points<-good.points<-timeline
            measured.points[this.times]<-1
            
            good.sel<-which(dens >= min.pres)
            good.points[good.sel]<-1
            for(j in (-min.run):min.run)
            {
                curr.sel<-good.sel+j
                curr.sel<-curr.sel[curr.sel>0 & curr.sel<=length(times)]
                good.points[curr.sel]<-1
            }
            
            measured.points<-measured.points*good.points
            to.keep[which(this.times %in% which(measured.points==1))]<-1
        }
        
        if(sum(to.keep)>0)
        {
            this.sel<-which(to.keep==1)
            this.new<-cbind(this.mass[this.sel],this.times[this.sel], this.intensi[this.sel], rep(this.grp, length(this.sel)))
            r.new<-nrow(this.new)
            height.rec[curr.label]<-max(this.intensi[this.sel])
            time.range.rec[curr.label]<-times[max(this.times[this.sel])]-times[min(this.times[this.sel])]
            mz.pres.rec[curr.label]<-length(this.sel)/(max(this.times[this.sel])-min(this.times[this.sel])+1)
            curr.label<-curr.label+1
            
            new.rec[rec.pointer:(rec.pointer+r.new-1),]<-this.new
            rec.pointer<-rec.pointer+r.new
            
        }
    }
    new.rec<-new.rec[1:(rec.pointer-1),]
    new.rec[,2]<-times[new.rec[,2]]
    results<-new('list')
    results$new.rec<-new.rec
    results$height.rec<-height.rec[1:(curr.label-1)]
    results$time.range.rec<-time.range.rec[1:(curr.label-1)]
    results$mz.pres.rec<-mz.pres.rec[1:(curr.label-1)]
    return(results)
}
