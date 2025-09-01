semi.sup.2d <-
function(folder, output_path, file.pattern=".cdf", known.table=NA, n.nodes=4, min.exp=2, mz.cut = 5e-5, rt.cut = 50,mz.search.range =  2e-4, rt.search.range = 200,  intensity.limit.quantile = 0.05, mPower=4, mz.tol=1e-5, subs=NULL, align.mz.tol=NA, align.chr.tol=NA, max.align.mz.diff=0.01, pre.process=FALSE, recover.mz.range=NA, recover.chr.range=NA, use.observed.range=TRUE, match.tol.ppm=NA, new.feature.min.count=2, recover.min.count=3, intensity.weighted=FALSE)
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
	
    suf<-paste(mz.cut, rt.cut,mz.search.range, rt.search.range, intensity.limit.quantile, mPower, sep="_")
    
    to.do<-paste(matrix(unlist(strsplit(tolower(files),"\\.")),nrow=2)[1,],suf, ".feature",sep="_")
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
                this.name<-paste(strsplit(tolower(files[j]),"\\.")[[1]][1],suf,".feature",sep="_")
                
                this.feature<-NA
                
                processable<-"goodgood"
                processable<-try(this.feature<-proc.cdf.2d(files[j], mz.cut = mz.cut, rt.cut = rt.cut, mz.search.range =  mz.search.range, rt.search.range = rt.search.range, mz.search.step = mz.search.range/4, rt.search.step = rt.search.range/4, intensity.limit.quantile = intensity.limit.quantile, bPlot = FALSE))
				
                if (inherits(processable, "try-error"))
                {
                    file.copy(from=files[j], to="error_files")
                    file.remove(files[j])
                }else{
                    save(this.feature,file=this.name)
                }
            }
        }
        stopCluster(cl)
    }

    all.files<-dir()
    sel<-which(files %in% all.files)
    files<-files[sel]
    
    features<-new("list")
    for(i in 1:length(files))
    {
        this.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf, ".feature",sep="_")
        message(this.name, " ")
        load(this.name)
        features[[i]]<-this.feature
    }
    
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

        message(c("***** correcting time, CPU time (seconds) ",as.vector(system.time(f2<-adjust.time(features,mz.tol=align.mz.tol, chr.tol=align.chr.tol,  max.align.mz.diff=max.align.mz.diff)))[1]))
        
        stopCluster(cl)
        save(f2,file=this.name)
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
    message("************************* merging to known peak table *********************************")
    if(is.na(match.tol.ppm)) match.tol.ppm<-aligned$mz.tol*1e6
    
    this.name<-paste("merge_done_", suf, ".bin", sep="")
    all.files<-dir()
    is.done<-all.files[which(all.files == this.name)]
    if(length(is.done)==0)
    {
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
                
                #message(i, ": sel.new ", sel.new, " , sel.known ", sel.known)
                
                #
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
        if(nrow(new.known.pairing)>0) sel<-sel[-(new.known.pairing[,2])]
        
        to.add.ftrs[,1]<-to.add.times[,1]<-known.table[sel, 6]
        to.add.ftrs[,2]<-to.add.times[,2]<-known.table[sel, 11]
        to.add.ftrs[,3]<-to.add.times[,3]<-known.table[sel, 9]
        to.add.ftrs[,4]<-to.add.times[,4]<-known.table[sel, 10]
        
        aligned.ftrs<-rbind(aligned$aligned.ftrs, to.add.ftrs)
        pk.times<-rbind(aligned$pk.times, to.add.times)
        new.known.pairing<-rbind(new.known.pairing, cbind(1:nrow(to.add.ftrs)+nrow(aligned$aligned.ftrs), sel))
        
        merged<-new("list")
        merged$aligned.ftrs<-aligned.ftrs
        merged$pk.times<-pk.times
        merged$new.known.pairing<-new.known.pairing
        
        save(merged, file=this.name)
        
    }else{
        
        load(this.name)
        aligned.ftrs<-merged$aligned.ftrs
        pk.times<-merged$pk.times
        new.known.pairing<-merged$new.known.pairing
    }
    gc()
    
    ###############################################################################################
    message("**************************** recovering weaker signals *******************************")
    suf<-paste(suf,recover.mz.range, recover.chr.range, use.observed.range,match.tol.ppm,new.feature.min.count,recover.min.count,sep="_")
    
    worklist<-paste(matrix(unlist(strsplit(tolower(files),"\\.")),nrow=2)[1,],suf,"semi_sup.recover",sep="_")
    to.do<-which(!(worklist %in% dir()))
    message(c("number of files to process: ", length(to.do)))
    
    if(length(to.do)>0)
    {
        grps<-round(seq(0, length(to.do), length=n.nodes+1))
        grps<-unique(grps)
        
        cl <- makeCluster(n.nodes)
        registerDoParallel(cl)
        #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
        clusterEvalQ(cl, library(apLCMS))

        features.recov<-foreach(i=2:length(grps)) %dopar%
        {
            this.subset<-to.do[(grps[i-1]+1):grps[i]]
            for(j in this.subset)
            {
                this.name<-paste(strsplit(tolower(files[j]),"\\.")[[1]][1],suf,"semi_sup.recover",sep="_")
                this.recovered<-recover.weaker(filename=files[j], loc=j, aligned.ftrs=aligned.ftrs, pk.times=pk.times, align.mz.tol=aligned$mz.tol, align.chr.tol=aligned$chr.tol, this.f1=features[[j]], this.f2=f2[[j]], mz.range=recover.mz.range, chr.range=recover.chr.range, use.observed.range=use.observed.range, orig.tol=mz.tol, bandwidth=.5, recover.min.count=recover.min.count)
                save(this.recovered, file=this.name)
            }
        }
        stopCluster(cl)
        gc()
    }
    

    ##############################################################################################
    message("loading feature tables after recovery")
    features.recov<-new("list")
    
    for(i in 1:length(files))
    {
        this.name<-paste(strsplit(tolower(files[i]),"\\.")[[1]][1],suf,"semi_sup.recover",sep="_")
        load(this.name)
        features.recov[[i]]<-this.recovered$this.f1
    }
    
    ##############################################################################################
    message("****************************** second round time correction *************************")
    suf<-paste(suf,"round 2",sep="_")
    this.name<-paste("time_correct_done_",suf,".bin",sep="")
    
    all.files<-dir()
    is.done<-all.files[which(all.files == this.name)]
    
    if(length(is.done)==0)
    {
        cl <- makeCluster(n.nodes)
        registerDoParallel(cl)
        #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
        clusterEvalQ(cl, library(apLCMS))

        message(c("***** correcting time, CPU time (seconds) ",as.vector(system.time(f2.recov<-adjust.time(features.recov, mz.tol=align.mz.tol, chr.tol=align.chr.tol, find.tol.max.d=10*mz.tol, max.align.mz.diff=max.align.mz.diff)))[1]))
        save(f2.recov,file=this.name)
        stopCluster(cl)
    }else{
        load(this.name)
    }
    gc()
    ###############################################################################################
    message("**************************** second round aligning features *************************")
    suf<-paste(suf,"min_exp", min.exp, 1,sep="_")
    this.name<-paste("aligned_done_",suf,".bin",sep="")
    all.files<-dir()
    is.done<-all.files[which(all.files == this.name)]
    if(length(is.done)==0)
    {
        cl <- makeCluster(n.nodes)
        registerDoParallel(cl)
        #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
        clusterEvalQ(cl, library(apLCMS))

        message(c("***** aligning features, CPU time (seconds): ", as.vector(system.time(aligned.recov<-feature.align(f2.recov, min.exp=min.exp,mz.tol=align.mz.tol,chr.tol=align.chr.tol, find.tol.max.d=10*mz.tol, max.align.mz.diff=max.align.mz.diff)))[1]))
        save(aligned.recov,file=this.name)
        stopCluster(cl)
    }else{
        load(this.name)
    }
    gc()
    
    ################## updating known.table ############
    ### notice aligned.ftrs contains all features from the known table and new data
    
    
    mass.d2<-mass.match(aligned.recov$aligned.ftrs[,1], known.table[,6],match.tol.ppm)
    mass.matched.pos<-which(mass.d2>0)
    
    known.assigned<-rep(0, nrow(known.table))
    new.assigned<-rep(0, nrow(aligned.recov$aligned.ftrs))
    new.known.pairing<-matrix(0, ncol=2, nrow=1)
    
    for(i in mass.matched.pos)
    {
        if(new.assigned[i] == 0)
        {
            #find all potentially related known/newly found peaks
            old.sel.new<-i
            this.mz.thres<-aligned.recov$aligned.ftrs[i,1]*match.tol.ppm/1e6
            sel.known<-which(abs(known.table[,6]-aligned.recov$aligned.ftrs[i,1]) < this.mz.thres)
            sel.new<-NULL
            for(m in 1:length(sel.known)) sel.new<-c(sel.new, which(abs(aligned.recov$aligned.ftrs[,1]-known.table[sel.known[m], 6]) < this.mz.thres))
            sel.known<-unique(sel.known)
            sel.new<-unique(sel.new)
            
            while(length(sel.new)>length(old.sel.new))
            {
                old.sel.new<-sel.new
                sel.known<-NULL
                for(m in 1:length(sel.new)) sel.known<-c(sel.known, which(abs(known.table[,6]-aligned.recov$aligned.ftrs[sel.new[m],1]) < this.mz.thres))
                sel.new<-NULL
                for(m in 1:length(sel.known)) sel.new<-c(sel.new, which(abs(aligned.recov$aligned.ftrs[,1]-known.table[sel.known[m], 6]) < this.mz.thres))
                sel.known<-unique(sel.known)
                sel.new<-unique(sel.new)
            }
            
            #
            time.matched<-mass.matched<-matrix(0, ncol=length(sel.new), nrow=length(sel.known))
            
            for(k in 1:length(sel.known))
            {
                time.matched[k,]<-abs(aligned.recov$aligned.ftrs[sel.new,2]-known.table[sel.known[k],11])
                mass.matched[k,]<-abs(known.table[sel.known[k],6]-aligned.recov$aligned.ftrs[sel.new,1])
            }
            mass.matched<-1*(mass.matched/median(known.table[sel.known,6]) <= match.tol.ppm*1e-6)
            time.matched[mass.matched == 0] <- 1e10
            
            
            time.matched[is.na(time.matched)]<-aligned$chr.tol/2-0.0000001
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
    
    known.2<-known.table
    known.num.experiments<-unique(known.2[,7])
    if(is.na(known.num.experiments)) known.num.experiments<-0
    new.num.experiments<-ncol(aligned.ftrs)-4+known.num.experiments
    
    if(nrow(new.known.pairing)>0)
    {
        
        for(i in 1:nrow(new.known.pairing))
        {
            known.2[new.known.pairing[i,2],]<-peak.characterize(existing.row=known.2[new.known.pairing[i,2],],ftrs.row=aligned.recov$aligned.ftrs[new.known.pairing[i,1],], chr.row=aligned.recov$pk.times[new.known.pairing[i,1],])
        }
        
        
        newly.found.ftrs<-which(!(1:nrow(aligned.recov$aligned.ftrs) %in% new.known.pairing[,1]))
        num.exp.found<-apply(aligned.recov$aligned.ftrs!=0, 1,sum)
        for(i in newly.found.ftrs)
        {
            if(num.exp.found[i] >= new.feature.min.count)
            {
                this.row<-peak.characterize(existing.row=NA,ftrs.row=aligned.recov$aligned.ftrs[i,], chr.row=aligned.recov$pk.times[i,])
                known.2<-rbind(known.2, this.row)
                new.known.pairing<-rbind(new.known.pairing, c(i,nrow(known.2)))
            }
        }
    }
    #################################################################################################
    
    rec<-new("list")
    colnames(aligned$aligned.ftrs)<-colnames(aligned$pk.times)<-colnames(aligned.recov$aligned.ftrs)<-colnames(aligned.recov$pk.times)<-c("mz","time","mz.min","mz.max",files)
    rec$features<-features.recov
    rec$features2<-f2.recov
    rec$aligned.ftrs<-aligned$aligned.ftrs
    rec$pk.times<-aligned$pk.times
    rec$final.ftrs<-aligned.recov$aligned.ftrs
    rec$final.times<-aligned.recov$pk.times
    rec$align.mz.tol<-aligned.recov$mz.tol
    rec$align.chr.tol<-aligned.recov$chr.tol
    rec$mz.tol<-mz.tol
    rec$updated.known.table<-known.2
    rec$ftrs.known.table.pairing<-new.known.pairing
    
    return(rec)
}
