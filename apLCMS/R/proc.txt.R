proc.txt <-
function(filename, output_path,min.pres=0.5, min.run=12,tol=NA, find.tol.maxd=1e-4, baseline.correct.noise.percentile=0.25, baseline.correct=0)
{
    oldpar <- par(no.readonly = TRUE)   
    on.exit(par(oldpar))
    par(mfrow=c(2,2))
    this.name<-paste(strsplit(tolower(filename),".txt")[[1]],"_",tol,"_",find.tol.maxd,".rawprof",sep="")
    all.files<-dir(output_path)
    is.there<-all.files[which(all.files == this.name)]
    
    if(length(is.there) > 0)
    {
        load(file.path(output_path, this.name))
        plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="tolerance level loaded",axes=FALSE)
        text(x=0,y=0,tol,cex=1.2)
    }else{
        ex.nc<-read.table(filename, header=TRUE, sep="\t")
        ex.nc<-as.matrix(ex.nc)
        ex.nc<-ex.nc[ex.nc[,3] != 0,]
        ex.nc<-ex.nc[order(ex.nc[,1], ex.nc[,2]),]
        
        this<-new("list")
        this$masses<-ex.nc[,2]
        this$intensi<-ex.nc[,3]
        this$labels<-ex.nc[,1]
        this$times<-unique(ex.nc[,1])
        
        raw.prof<-adaptive.bin(this, min.run=min.run, min.pres=min.pres, tol=tol, baseline.correct=baseline.correct)
        save(raw.prof, file=file.path(output_path, this.name))
    }
    
    newprof<-cbind(raw.prof$masses, raw.prof$labels, raw.prof$intensi, raw.prof$grps)
    h.1<-log10(raw.prof$height.rec[raw.prof$height.rec[,2]<= max(2, raw.prof$min.count.run*min.pres/2),3])
    h.2<-log10(raw.prof$height.rec[raw.prof$height.rec[,2]>= raw.prof$min.count.run*min.pres,3])
    if(length(h.1)>50)
    {
        plot(density(h.1),xlab="maximum height of group (log scale)", xlim=range(c(h.1, h.2)),main="Black - noise groups \n Blue - selected groups")
    }else{
        plot(NA,NA,xlab="maximum height of group (log scale)", xlim=range(c(h.1, h.2)), ylim=c(0,1), main="Black - noise groups \n Blue - selected groups")
        if(length(h.1)>0)	abline(v=h.1)
    }
    
    if(is.na(baseline.correct))
    {
        baseline.correct<-10^quantile(h.1, baseline.correct.noise.percentile)
        message(c("maximal height cut is automatically set at the", baseline.correct.noise.percentile, "percentile of noise group heights: ", baseline.correct))
    }else{
        message(c("maximal height cut is provided by user: ", baseline.correct))
    }
    abline(v=log10(baseline.correct), col="red")
    
    if(is.na(baseline.correct)) baseline.correct<-0
    run.sel<-raw.prof$height.rec[which(raw.prof$height.rec[,2] >= raw.prof$min.count.run*min.pres & raw.prof$height.rec[,3]>baseline.correct),1]
    newprof<-newprof[newprof[,4] %in% run.sel,]
    new.prof<-cont.index(newprof,  min.pres=min.pres, min.run=min.run)
    lines(density(log10(new.prof$height.rec)),col="blue")
    time.range.rec<-new.prof$time.range.rec
    mz.pres.rec<-new.prof$mz.pres.rec
    hist(time.range.rec, xlab="Range of retention time in the same group", ylab="Density",freq=FALSE,nclass=100,main="Group retention time range distribution")
    hist(mz.pres.rec, xlab="% signal present in the same group", ylab="Density" ,freq=FALSE, nclass=20, main="Group % present signal distribution")
    
    return(new.prof$new.rec)
}
