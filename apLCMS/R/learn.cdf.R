learn.cdf <-
function(filename, output_path, tol=2e-5, min.run=4, min.pres=0.3, baseline.correct=0, ridge.smoother.window=50, smoother.window=c(1, 5, 10), known.mz, match.tol.ppm=5, do.plot=FALSE, pos.confidence=0.99, neg.confidence=0.99, max.ftrs.to.use=10, do.grp.reduce=TRUE, remove.bottom.ftrs=0, max.fpr=seq(0,0.6, by=0.1), min.tpr=seq(0.8, 1,by=0.1), intensity.weighted=FALSE)
{
    if(do.plot){
      oldpar <- par(no.readonly = TRUE)   
      on.exit(par(oldpar))
      par(mfrow=c(2,2))
    }
    
    this.name<-paste(strsplit(tolower(filename),"\\.")[[1]][1], "_", tol,"_",ridge.smoother.window, "_",baseline.correct, ".rawlearn",sep="")
    all.files<-dir(output_path)
    is.there<-all.files[which(all.files == this.name)]
    
    if(length(is.there) > 0)
    {
        load(file.path(output_path, this.name))
        if(do.plot)
        {
            plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="tolerance level loaded",axes=FALSE)
            text(x=0,y=0,tol,cex=1.2)
        }
    }else{
        this<-load.lcms(filename)
        raw.prof<-adaptive.bin.2(this, tol=tol, ridge.smoother.window=ridge.smoother.window, baseline.correct=baseline.correct)
        save(raw.prof, file=file.path(output_path, this.name))
    }
    
    if(is.na(baseline.correct)) baseline.correct<-0
    run.sel<-raw.prof$height.rec[which(raw.prof$height.rec[,2] >= min.run*min.pres & raw.prof$height.rec[,3]>baseline.correct),1]
    newprof<-cbind(raw.prof$masses, raw.prof$labels, raw.prof$intensi, raw.prof$grps)
    newprof<-newprof[newprof[,4] %in% run.sel,]
    raw.prof$height.rec<-raw.prof$height.rec[raw.prof$height.rec[,1] %in% newprof[,4],]
    
    new.prof<-cont.index(newprof,  min.pres=min.pres, min.run=min.run)
    raw.prof$height.rec<-raw.prof$height.rec[raw.prof$height.rec[,1] %in%  new.prof$new.rec[,4],]
    raw.prof$masses<-new.prof$new.rec[,1]
    raw.prof$labels<-new.prof$new.rec[,2]
    raw.prof$intensi<-new.prof$new.rec[,3]
    raw.prof$grps<-new.prof$new.rec[,4]
    raw.prof$height.rec[,3]<-new.prof$height.rec
    
    eic.rec.0<-eic.disect(raw.prof, smoother.window=smoother.window)
    prof<-eic.rec.0$prof
    eic.rec.0<-eic.rec.0$eic.ftrs
    
    a<-eic.pred(eic.rec.0, known.mz,to.use=max.ftrs.to.use, match.tol.ppm=match.tol.ppm, do.grp.reduce=do.grp.reduce, remove.bottom=remove.bottom.ftrs, max.fpr=max.fpr[1], min.tpr=min.tpr[1], do.plot=do.plot)
    
    
    fpr.tpr.combo<-expand.grid(max.fpr, min.tpr)
    prof.sel<-new("list")
    
    for(i in 1:nrow(fpr.tpr.combo))
    {
        chosen<-a$matched*0
        chosen[a$matched==1 & a$tpr<=fpr.tpr.combo[i,2]]<-1
        chosen[a$matched==0 & a$fpr<=fpr.tpr.combo[i,1]]<-1
        
        selected<-which(chosen == 1)
        prof.sel[[i]]<-prof[which(prof[,4] %in% raw.prof$height.rec[selected,1]),]
        names(prof.sel)[[i]]<-paste("fpr", fpr.tpr.combo[i,1], "tpr", fpr.tpr.combo[i,2],sep="_")
    }
    prof.sel$fpr.tpr.combo<-fpr.tpr.combo
    prof.sel$model.detail<-a
    
    return(prof.sel)
}
