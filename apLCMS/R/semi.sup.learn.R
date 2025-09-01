semi.sup.learn <-
function(folder, output_path, file.pattern=".cdf", known.table=NA, n.nodes=4, min.exp=2, min.pres=0.3, min.run=4, mz.tol=1e-5, shape.model="bi-Gaussian",  baseline.correct=0, peak.estim.method="moment", min.bw=NA, max.bw=NA, sd.cut=c(0.01,500), component.eliminate=0.01, moment.power=1, sigma.ratio.lim=c(0.01, 100), subs=NULL, align.mz.tol=NA, align.chr.tol=NA, max.align.mz.diff=0.01, pre.process=FALSE, recover.mz.range=NA, recover.chr.range=NA, use.observed.range=TRUE, match.tol.ppm=5, new.feature.min.count=2, recover.min.count=3, use.learn=TRUE, ridge.smoother.window=50, smoother.window=c(1, 5, 10),pos.confidence=0.99, neg.confidence=0.99, max.ftrs.to.use=10, do.grp.reduce=TRUE, remove.bottom.ftrs=0, max.fpr=0.5, min.tpr=0.9, intensity.weighted=FALSE)
{
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(folder)
    
    files<-dir(pattern=file.pattern, ignore.case = TRUE)
    files<-files[order(files)]
    if(!is.null(subs))
    {
        if(!is.na(subs[1])) files<-files[subs]
    }
    
    file.copy(from=files, to=output_path)
    setwd(output_path)
    
    ###############################################################################################
    
    try(dir.create("error_files"), silent = TRUE)
    message("***************************** prifiles --> feature lists *****************************")
    suf.prof<-paste(min.pres,min.run,mz.tol,baseline.correct,sep="_")
    if(use.learn) suf.prof<-paste(min.run, min.pres, ridge.smoother.window, mz.tol, baseline.correct, max.ftrs.to.use, do.grp.reduce, remove.bottom.ftrs, max.fpr, min.tpr, sep="_")
    suf<-paste(suf.prof, shape.model, min.run, min.pres, sd.cut[1], sd.cut[2],component.eliminate, moment.power, sep="_")
    if(shape.model=="bi-Gaussian") suf<-paste(suf, sigma.ratio.lim[1], sigma.ratio.lim[2],sep="_")
    
    cl <- makeCluster(n.nodes)
    registerDoParallel(cl)
    #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
    clusterEvalQ(cl, library(apLCMS))

    features<-foreach(i=1:length(files)) %dopar%
    {
        this.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf, min.bw, max.bw,".feature",sep="_")
        all.files<-dir()
        do.exist<-all.files[which(all.files == this.name)]
        
        if(length(do.exist)==0)
        {
            this.feature<-NA
            that.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf.prof,".profile",sep="_")
            all.files<-dir()
            that.exist<-all.files[which(all.files == that.name)]
            
            processable<-"goodgood"
            if(length(that.exist)==0)
            {
                if(use.learn)
                {
                    message(c(i, "  ", that.name, "  ", as.vector(system.time(processable<-try(this.prof<-learn.cdf(files[i], output_path, known.mz=known.table[,6], tol=mz.tol, min.run=min.run, min.pres=min.pres, match.tol.ppm=match.tol.ppm, baseline.correct=baseline.correct, ridge.smoother.window=ridge.smoother.window, smoother.window=smoother.window,pos.confidence=pos.confidence, neg.confidence=neg.confidence, max.ftrs.to.use=max.ftrs.to.use, do.grp.reduce=do.grp.reduce, remove.bottom.ftrs=remove.bottom.ftrs, max.fpr=max.fpr, min.tpr=min.tpr, intensity.weighted=intensity.weighted))[1]))[1]))
                }else{
                    message(c(i, "  ", that.name, "  ", as.vector(system.time(processable<-try(this.prof<-proc.cdf(files[i], output_path, min.pres=min.pres, min.run=min.run, tol=mz.tol, baseline.correct=baseline.correct, do.plot=FALSE, intensity.weighted=intensity.weighted))[1]))[1]))
                }
              if (inherits(processable, "try-error"))
                {
                    file.copy(from=files[i], to="error_files")
                    file.remove(files[i])
                }else{
                    save(this.prof,file=that.name)
                }
            }else{
                message(paste(i, "  loading: ", that.name))
                load(that.name)
            }
            
            if (!inherits(processable, "try-error"))
            {
                processable.2<-"goodgood"
                message(c(i, "  ", this.name,"  ",as.vector(system.time(processable.2<-try(this.feature<-prof.to.features(this.prof[[1]], min.bw=min.bw, max.bw=max.bw, sd.cut=sd.cut, shape.model=shape.model, estim.method=peak.estim.method, do.plot=FALSE, component.eliminate=component.eliminate, power=moment.power))))[1]))
                
                if (inherits(processable.2, "try-error"))
                {
                    file.copy(from=files[i], to="error_files")
                    file.remove(files[i])
                    this.feature<-NA
                }else{
                    save(this.feature, file=this.name)
                }
            }
        }else{
            message(paste(i, "  loading: ", this.name))
            load(this.name)
        }
        this.feature
    }
    stopCluster(cl)

    all.files<-dir()
    sel<-which(files %in% all.files)
    files<-files[sel]
    features<-features[sel]
    
    
    ###############################################################################################
    message("****************************** time correction ***************************************")
    suf<-paste(suf,align.mz.tol,align.chr.tol,subs[1],subs[length(subs)],sep="_")
    this.name<-paste("time_correct_done_",suf,".bin",sep="")
    
    all.files<-dir()
    is.done<-all.files[which(all.files == this.name)]
    
    if(length(is.done)==0)
    {
        cl <- makeCluster(n.nodes)
        registerDoParallel(cl)
        #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
        clusterEvalQ(cl, library(apLCMS))

        if(is.na(align.mz.tol)) align.mz.tol=2 * match.tol.ppm *1e-6
        message(c("***** correcting time, CPU time (seconds) ",as.vector(system.time(f2<-adjust.time(features,mz.tol=align.mz.tol, chr.tol=align.chr.tol, find.tol.max.d=10*mz.tol, max.align.mz.diff=max.align.mz.diff)))[1]))
        save(f2,file=this.name)
        stopCluster(cl)
    }else{
        load(this.name)
    }
    gc()
    
    ###############################################################################################
    message("****************************  aligning features **************************************")
    suf<-paste(suf,min.exp,sep="_")
    this.name<-paste("aligned_done_",suf,".bin",sep="")
    all.files<-dir()
    is.done<-all.files[which(all.files == this.name)]
    if(length(is.done)==0)
    {
        cl <- makeCluster(n.nodes)
        registerDoParallel(cl)
        #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
        clusterEvalQ(cl, library(apLCMS))

        if(is.na(align.mz.tol)) align.mz.tol=2 * match.tol.ppm *1e-6
        message(c("***** aligning features, CPU time (seconds): ", as.vector(system.time(aligned<-feature.align(f2, min.exp=min.exp,mz.tol=align.mz.tol,chr.tol=align.chr.tol, find.tol.max.d=10*mz.tol, max.align.mz.diff=max.align.mz.diff)))[1]))
        save(aligned,file=this.name)
        stopCluster(cl)
    }else{
        load(this.name)
    }
    gc()
    
    ###############################################################################################
    message("merging to known peak table")
    if(is.na(match.tol.ppm)) match.tol.ppm<-aligned$mz.tol*1e6
    
    mass.d2<-mass.match(aligned$aligned.ftrs[,1], known.table[,6],match.tol.ppm)
    mass.matched.pos<-which(mass.d2>0)
    
    known.assigned<-rep(0, nrow(known.table))
    new.assigned<-rep(0, nrow(aligned$aligned.ftrs))
    new.known.pairing<-matrix(0, ncol=2, nrow=1)
    
    for(i in mass.matched.pos)
    {
        if(new.assigned[i] == 0)
        {
            #find all potentially related known/newly found peaks
            old.sel.new<-i
            this.mz.thres<-aligned$aligned.ftrs[i,1]*match.tol.ppm/1e6
            sel.known<-which(abs(known.table[,6]-aligned$aligned.ftrs[i,1]) < this.mz.thres)
            sel.new<-NULL
            for(m in 1:length(sel.known)) sel.new<-c(sel.new, which(abs(aligned$aligned.ftrs[,1]-known.table[sel.known[m], 6]) < this.mz.thres))
            sel.known<-unique(sel.known)
            sel.new<-unique(sel.new)
            
            while(length(sel.new)>length(old.sel.new))
            {
                old.sel.new<-sel.new
                sel.known<-NULL
                for(m in 1:length(sel.new)) sel.known<-c(sel.known, which(abs(known.table[,6]-aligned$aligned.ftrs[sel.new[m],1]) < this.mz.thres))
                sel.new<-NULL
                for(m in 1:length(sel.known)) sel.new<-c(sel.new, which(abs(aligned$aligned.ftrs[,1]-known.table[sel.known[m], 6]) < this.mz.thres))
                sel.known<-unique(sel.known)
                sel.new<-unique(sel.new)
            }
            
            time.matched<-mass.matched<-matrix(0, ncol=length(sel.new), nrow=length(sel.known))
            
            for(k in 1:length(sel.known))
            {
                time.matched[k,]<-abs(aligned$aligned.ftrs[sel.new,2]-known.table[sel.known[k],11])
                mass.matched[k,]<-abs(known.table[sel.known[k],6]-aligned$aligned.ftrs[sel.new,1])
            }
            mass.matched<-1*(mass.matched/median(known.table[sel.known,6]) <= match.tol.ppm*1e-6)
            time.matched[mass.matched == 0] <- 1e10
            
            
            time.matched[is.na(time.matched)]<-aligned$chr.tol/2
            both.matched<-find.match(time.matched, unacceptable=aligned$chr.tol/2)
            
            for(m in 1:length(sel.new))
            {
                k<-which(both.matched[,m]==1)
                if(length(k)==1)
                {
                    if(known.assigned[sel.known[k]]==0)
                    {
                        new.assigned[sel.new[m]]<-1
                        known.assigned[sel.known[k]]<-1
                        new.known.pairing<-rbind(new.known.pairing, c(sel.new[m], sel.known[k]))
                    }
                }
            }
        }
    }
    colnames(new.known.pairing)<-c("new","known")
    new.known.pairing<-new.known.pairing[-1,]
    
    to.add.ftrs<-matrix(0, ncol=ncol(aligned$aligned.ftrs), nrow=nrow(known.table)-nrow(new.known.pairing))
    to.add.times<-matrix(NA, ncol=ncol(aligned$aligned.ftrs), nrow=nrow(known.table)-nrow(new.known.pairing))
    sel<-1:nrow(known.table)
    sel<-sel[-(new.known.pairing[,2])]
    
    to.add.ftrs[,1]<-to.add.times[,1]<-known.table[sel, 6]
    to.add.ftrs[,2]<-to.add.times[,2]<-known.table[sel, 11]
    to.add.ftrs[,3]<-to.add.times[,3]<-known.table[sel, 9]
    to.add.ftrs[,4]<-to.add.times[,4]<-known.table[sel, 10]
    
    aligned.ftrs<-rbind(aligned$aligned.ftrs, to.add.ftrs)
    pk.times<-rbind(aligned$pk.times, to.add.times)
    new.known.pairing<-rbind(new.known.pairing, cbind(1:nrow(to.add.ftrs)+nrow(aligned$aligned.ftrs), sel))
    gc()
    
    
    ###############################################################################################
    message("**************************** recovering weaker signals *******************************")
    suf<-paste(suf,recover.mz.range, recover.chr.range, use.observed.range,match.tol.ppm,new.feature.min.count,recover.min.count,sep="_")
    
    cl <- makeCluster(n.nodes)
    registerDoParallel(cl)
    #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
    clusterEvalQ(cl, library(apLCMS))

    foreach(i=1:length(files)) %dopar%
    {
        this.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf,"semi_sup.recover",sep="_")
        all.files<-dir()
        do.exist<-all.files[which(all.files == this.name)]
        
        if(length(do.exist)==0)
        {
            this.recovered<-recover.weaker(filename=files[i], loc=i, aligned.ftrs=aligned.ftrs, pk.times=pk.times, align.mz.tol=aligned$mz.tol, align.chr.tol=aligned$chr.tol, this.f1=features[[i]], this.f2=f2[[i]], mz.range=recover.mz.range, chr.range=recover.chr.range, use.observed.range=use.observed.range, orig.tol=mz.tol, min.bw=min.bw, max.bw=max.bw, bandwidth=.5, recover.min.count=recover.min.count)
            save(this.recovered, file=this.name)
        }
    }
    stopCluster(cl)

    new.aligned<-aligned
    new.aligned$orig.known.table<-known.table
    
    for(i in 1:length(files))
    {
        this.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf,"semi_sup.recover",sep="_")
        load(this.name)
        aligned.ftrs[,i+4]<-this.recovered$this.ftrs
        pk.times[,i+4]<-this.recovered$this.times
        new.aligned$features[[i]]<-this.recovered$this.f1
        new.aligned$f2[[i]]<-this.recovered$this.f2
    }
    
    new.pk.times<-apply(pk.times[,-1:-4], 1, mean,na.rm=T)
    pk.times[is.na(pk.times[,2]),2]<-new.pk.times[is.na(pk.times[,2])]
    aligned.ftrs[,2]<-pk.times[,2]
    new.aligned$aligned.ftrs<-aligned.ftrs
    new.aligned$pk.times<-pk.times
    
    
    ################## updating known.table ############
    ### notice aligned.ftrs contains all features from the known table and new data
    
    known.2<-known.table
    num.exp.found<-apply(aligned.ftrs[,5:ncol(aligned.ftrs)]!=0, 1, sum)
    known.num.experiments<-unique(known.2[,7])
    new.num.experiments<-ncol(aligned.ftrs)-4+known.num.experiments
    
    for(i in 1:nrow(new.known.pairing))
    {
        if(num.exp.found[new.known.pairing[i,1]] >= 1)
        {
            known.2[new.known.pairing[i,2],]<-peak.characterize(existing.row=known.2[new.known.pairing[i,2],],ftrs.row=aligned.ftrs[new.known.pairing[i,1],], chr.row=pk.times[new.known.pairing[i,1],])
        }else{
            known.2[new.known.pairing[i,2],c(7,8)]<-c(new.num.experiments, known.num.experiments*known.2[new.known.pairing[i,2],8]/new.num.experiments)
        }
    }
    
    newly.found.ftrs<-which(!(1:nrow(aligned.ftrs) %in% new.known.pairing[,1]))
    for(i in newly.found.ftrs)
    {
        if(num.exp.found[i] >= new.feature.min.count)
        {
            this.row<-peak.characterize(existing.row=NA,ftrs.row=aligned.ftrs[i,], chr.row=pk.times[i,])
            #			this.row[c(7,8)]<-c(new.num.experiments, this.row[7]*this.row[8]/new.num.experiments)
            known.2<-rbind(known.2, this.row)
            new.known.pairing<-rbind(new.known.pairing, c(i,nrow(known.2)))
        }
    }
    
    #################################################################################################
    sel<-which(num.exp.found >= min.exp)
    new.aligned$aligned.ftrs<-new.aligned$aligned.ftrs[sel,]
    new.aligned$pk.times<-new.aligned$pk.times[sel,]
    missed.loc<-(1:nrow(aligned.ftrs))[!(1:nrow(aligned.ftrs) %in% new.known.pairing[,1])]
    missed.loc<-cbind(missed.loc, rep(NA, length(missed.loc)))
    new.known.pairing<-rbind(new.known.pairing, missed.loc)
    new.known.pairing<-new.known.pairing[order(new.known.pairing[,1]),]
    new.known.pairing<-new.known.pairing[sel,]
    new.known.pairing[,1]<-1:nrow(new.known.pairing)
    
    rec<-new("list")
    colnames(aligned$aligned.ftrs)<-colnames(aligned$pk.times)<-colnames(new.aligned$aligned.ftrs)<-colnames(new.aligned$pk.times)<-c("mz","time","mz.min","mz.max",files)
    rec$features<-new.aligned$features
    rec$features2<-new.aligned$f2
    rec$aligned.ftrs<-aligned$aligned.ftrs
    rec$pk.times<-aligned$pk.times
    rec$final.ftrs<-new.aligned$aligned.ftrs
    rec$final.times<-new.aligned$pk.times
    rec$align.mz.tol<-new.aligned$mz.tol
    rec$align.chr.tol<-new.aligned$chr.tol
    rec$mz.tol<-mz.tol
    rec$updated.known.table<-known.2
    rec$ftrs.known.table.pairing<-new.known.pairing
    
    return(rec)
    
}
