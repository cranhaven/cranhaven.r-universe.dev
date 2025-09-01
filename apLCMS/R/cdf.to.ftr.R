cdf.to.ftr <-
function(folder, output_path, file.pattern=".cdf", n.nodes=4, min.exp=2, min.pres=0.5, min.run=12, mz.tol=1e-5, baseline.correct.noise.percentile=0.05, shape.model="bi-Gaussian",  BIC.factor=2, baseline.correct=0, peak.estim.method="moment", min.bw=NA, max.bw=NA, sd.cut=c(0.01,500), sigma.ratio.lim=c(0.01, 100), component.eliminate=0.01, moment.power=1, subs=NULL, align.mz.tol=NA, align.chr.tol=NA, max.align.mz.diff=0.01, pre.process=FALSE, recover.mz.range=NA, recover.chr.range=NA, use.observed.range=TRUE,recover.min.count=3, intensity.weighted=FALSE)
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
    suf<-paste(suf.prof, shape.model, sd.cut[1], sd.cut[2],component.eliminate, moment.power, sep="_")
    if(shape.model=="bi-Gaussian") suf<-paste(suf, sigma.ratio.lim[1], sigma.ratio.lim[2],sep="_")
    
    to.do<-paste(matrix(unlist(strsplit(tolower(files),"\\.")),nrow=2)[1,],suf, min.bw, max.bw,".feature",sep="_")
    to.do<-which(!(to.do %in% dir()))
    message(c("number of files to process: ", length(to.do)))
    
    if(length(to.do)>0)
    {
        grps<-round(seq(0, length(to.do), length=n.nodes+1))
        grps<-unique(grps)
        
        cl <- makeCluster(n.nodes)
        registerDoParallel(cl)
        #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
        clusterEvalQ(cl, library(apLCMS))
        
        
        features<-foreach(i=2:length(grps)) %dopar%
        {
            this.subset<-to.do[(grps[i-1]+1):grps[i]]
            for(j in this.subset)
            {
                this.name<-paste(strsplit(tolower(files[j]),"\\.")[[1]][1],suf, min.bw, max.bw,".feature",sep="_")
                
                this.feature<-NA
                that.name<-paste(strsplit(tolower(files[j]),"\\.")[[1]][1],suf.prof,".profile",sep="_")
                
                processable<-"goodgood"
                processable<-try(this.prof<-proc.cdf(files[j], output_path, min.pres=min.pres, min.run=min.run, tol=mz.tol, baseline.correct=baseline.correct, baseline.correct.noise.percentile=baseline.correct.noise.percentile, do.plot=FALSE, intensity.weighted=intensity.weighted))
                if (inherits(processable, "try-error"))
                {
                    file.copy(from=files[j], to="error_files")
                    file.remove(files[j])
                }else{
                    save(this.prof,file=that.name)
                }
                
                if (!inherits(processable, "try-error"))
                {
                    processable.2<-"goodgood"
                    processable.2<-try(this.feature<-prof.to.features(this.prof, min.bw=min.bw, max.bw=max.bw, sd.cut=sd.cut, shape.model=shape.model, estim.method=peak.estim.method, do.plot=FALSE, component.eliminate=component.eliminate, power=moment.power, BIC.factor=BIC.factor))
                    
                    if (inherits(processable.2, "try-error"))
                    {
                        file.copy(from=files[j], to="error_files")
                        file.remove(files[j])
                        this.feature<-NA
                    }else{
                        save(this.feature, file=this.name)
                    }
                }
            }
            1
        }
        stopCluster(cl)
        
    }
    
    all.files<-dir()
    sel<-which(files %in% all.files)
    files<-files[sel]
    
    features<-new("list")
    for(i in 1:length(files))
    {
        this.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf, min.bw, max.bw,".feature",sep="_")
        message(this.name, " ")
        load(this.name)
        features[[i]]<-this.feature
    }
    
    gc()
    
    if(!pre.process)
    {
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
            
            message(c("***** aligning features, CPU time (seconds): ", as.vector(system.time(aligned<-feature.align(f2, min.exp=min.exp,mz.tol=align.mz.tol,chr.tol=align.chr.tol, find.tol.max.d=10*mz.tol, max.align.mz.diff=max.align.mz.diff)))[1]))
            save(aligned,file=this.name)
            stopCluster(cl)
            
        }else{
            load(this.name)
        }
        gc()
        
        ###############################################################################################
        message("**************************** recovering weaker signals *******************************")
        suf<-paste(suf,recover.mz.range, recover.chr.range, use.observed.range,sep="_")
        
        worklist<-paste(matrix(unlist(strsplit(tolower(files),"\\.")),nrow=2)[1,],suf,".recover",sep="_")
        to.do<-which(!(worklist %in% dir()))
        grps<-round(seq(0, length(to.do), length=n.nodes+1))
        grps<-unique(grps)
        
        message(c("number of files to process: ", length(to.do)))
        
        if(length(to.do)>0)
        {
            cl <- makeCluster(n.nodes)
            registerDoParallel(cl)
            #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
            clusterEvalQ(cl, library(apLCMS))
            
            
            features.recov<-foreach(i=2:length(grps)) %dopar%
            {
                this.subset<-to.do[(grps[i-1]+1):grps[i]]
                for(j in this.subset)
                {
                    this.name<-paste(strsplit(tolower(files[j]),"\\.")[[1]][1],suf,".recover",sep="_")
                    this.recovered<-recover.weaker(filename=files[j], loc=j, aligned.ftrs=aligned$aligned.ftrs, pk.times=aligned$pk.times, align.mz.tol=aligned$mz.tol, align.chr.tol=aligned$chr.tol, this.f1=features[[j]], this.f2=f2[[j]], mz.range=recover.mz.range, chr.range=recover.chr.range, use.observed.range=use.observed.range, orig.tol=mz.tol, min.bw=min.bw, max.bw=max.bw, bandwidth=.5, recover.min.count=recover.min.count)
                    save(this.recovered, file=this.name)
                }
            }
            stopCluster(cl)
            gc()
        }
        
        new.aligned<-aligned
        for(i in 1:length(files))
        {
            this.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf,".recover",sep="_")
            load(this.name)
            new.aligned$aligned.ftrs[,i+4]<-this.recovered$this.ftrs
            new.aligned$pk.times[,i+4]<-this.recovered$this.times
            new.aligned$features[[i]]<-this.recovered$this.f1
            new.aligned$f2[[i]]<-this.recovered$this.f2
            gc()
        }
        
        #################################################################################################
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
        
        return(rec)
    }
}
