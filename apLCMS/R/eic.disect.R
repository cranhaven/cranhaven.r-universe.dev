eic.disect <-
function(raw.prof, smoother.window=c(1, 5, 10))
{
    newprof<-cbind(raw.prof$masses, raw.prof$labels, raw.prof$intensi, raw.prof$grps)
    height.rec<-raw.prof$height.rec
    
    labels<-newprof[,2]
    times<-unique(labels)
    times<-times[order(times)]
    time.points<-length(times)
    
    for(i in 1:length(times)) labels[which(newprof[,2]==times[i])]<-i  #now labels is the index of time points
    newprof[,2]<-labels
    
    times<-times[order(times)]
    l<-nrow(newprof)
    timeline<-rep(0,time.points)
    aver.time.range<-(max(times)-min(times))/time.points
    
    grps<-newprof[,4]
    curr.label<-1
    
    newprof<-newprof[order(newprof[,4], newprof[,1]),]
    r.newprof<-nrow(newprof)
    breaks<-c(0, which(newprof[1:(r.newprof-1),4] != newprof[2:r.newprof,4]), r.newprof)
    
    my.dens<-function(x, times, w=NULL, bw, kernel="gaussian")
    {
        if(is.null(w[1])) w<-rep(1, length(x))
        d<-density(x, weights=w/sum(w), bw=bw, kernel=kernel)
        d$y<-d$y * sum(w)
        d2<-approx(d$x, d$y, xout=times, rule=2)
        d2
    }
    
    new.times<-1:length(times)
    all.smoother.window<-smoother.window
    
    for(mm in 1:length(all.smoother.window))
    {
        smoother.window<-all.smoother.window[mm]
        eic.rec<-matrix(0, nrow=length(breaks)-1, ncol=44)
        this.names<-c("grp.label", "mz", "run.length", "raw.density.0","raw.density.25","raw.density.50","raw.density.75", "raw.density.100","raw.density.100.overall","b.raw.density.0","b.raw.density.25","b.raw.density.50","b.raw.density.75", "b.raw.density.100","b.raw.density.100.overall", "signal.height.0", "signal.height.25", "signal.height.50", "signal.height.75", "signal.height.100", "wtd.density.0", "wtd.density.25", "wtd.density.50", "wtd.density.75", "wtd.density.100","wtd.density.100.overall","b.wtd.density.0", "b.wtd.density.25", "b.wtd.density.50", "b.wtd.density.75", "b.wtd.density.100","b.wtd.density.100.overall", "log.w.density.0", "log.w.density.25", "log.w.density.50", "log.w.density.75", "log.w.density.100","log.w.density.100.overall", "b.log.w.density.0", "b.log.w.density.25", "b.log.w.density.50", "b.log.w.density.75", "b.log.w.density.100","b.log.w.density.100.overall")
        this.names[-c(1,2,3,16:20)]<-paste("bw", smoother.window, this.names[-c(1,2,3,16:20)],sep=".")
        colnames(eic.rec)<-this.names
        
        this.time<-round(median(new.times))
        single.point.dens<-my.dens(this.time, times=new.times, bw=smoother.window,kernel="gaussian")
        single.point.dens<-c(quantile(single.point.dens$y[this.time], c(0, 0.25, 0.5, 0.75, 1)), max(single.point.dens$y))
        
        single.point.dens.2<-my.dens(this.time, times=new.times, bw=smoother.window,kernel="rectangular")
        single.point.dens.2<-c(quantile(single.point.dens.2$y[this.time], c(0, 0.25, 0.5, 0.75, 1)), max(single.point.dens.2$y))
        
        for(m in 2:length(breaks))
        {
            this.prof<-newprof[(breaks[m-1]+1):breaks[m],]
            if(is.null(nrow(this.prof)))
            {
                eic.rec[m-1,]<-c(this.prof[4], this.prof[1], 1, single.point.dens, single.point.dens.2, rep(this.prof[3],5), single.point.dens*this.prof[3],single.point.dens.2*this.prof[3], single.point.dens*log10(1+this.prof[3]),single.point.dens.2*log10(1+this.prof[3]))
            }else{
                
                this.prof<-this.prof[order(this.prof[,2]),]
                this.times<-this.prof[,2]
                this.intensi<-this.prof[,3]
                this.mass<-this.prof[,1]
                this.label<-1
                
                dens<-my.dens(x=this.times, times=new.times, bw=smoother.window)
                dens<-c(quantile(dens$y[this.times], c(0, 0.25, 0.5, 0.75, 1)), max(dens$y))
                dens.box<-my.dens(x=this.times, times=new.times, bw=smoother.window, kernel="rectangular")
                dens.box<-c(quantile(dens.box$y[this.times], c(0, 0.25, 0.5, 0.75, 1)), max(dens.box$y))
                
                y<-this.intensi
                dens2<-my.dens(x=this.times, times=new.times, bw=smoother.window, w=y)
                dens2<-c(quantile(dens2$y[this.times], c(0, 0.25, 0.5, 0.75, 1)), max(dens2$y))
                dens2.box<-my.dens(x=this.times, times=new.times, bw=smoother.window, w=y, kernel="rectangular")
                dens2.box<-c(quantile(dens2.box$y[this.times], c(0, 0.25, 0.5, 0.75, 1)), max(dens2.box$y))
                
                y<-log10(1+this.intensi)
                dens3<-my.dens(x=this.times, times=new.times, bw=smoother.window, w=y)
                dens3<-c(quantile(dens3$y[this.times], c(0, 0.25, 0.5, 0.75, 1)), max(dens3$y))
                dens3.box<-my.dens(x=this.times, times=new.times, bw=smoother.window, w=y, kernel="rectangular")
                dens3.box<-c(quantile(dens3.box$y[this.times], c(0, 0.25, 0.5, 0.75, 1)), max(dens3.box$y))
                
                eic.rec[m-1,]<-c(this.prof[1,4], median(this.prof[,1]), diff(range(this.prof[,2]))+1, dens, dens.box, quantile(this.prof[,3], c(0, 0.25, 0.5, 0.75, 1)), dens2, dens2.box, dens3, dens3.box)
            }
        }
        if(mm == 1)
        {
            all.eic.rec<-eic.rec
        }else{
            all.eic.rec<-cbind(all.eic.rec, eic.rec[,-c(1,2,3,16:20)])
        }
    }
    
    newprof[,2]<-times[newprof[,2]]
    all.eic.rec<-cbind(all.eic.rec, height.rec[,c(-1, -3)])
    return(list(prof=newprof, eic.ftrs=all.eic.rec))
}
