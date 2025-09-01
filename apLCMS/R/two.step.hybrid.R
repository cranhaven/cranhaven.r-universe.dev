
two.step.hybrid <-function(folder, info, min.within.batch.prop.detect=0.4, min.within.batch.prop.report=0.5, min.batch.prop=0.5, batch.align.mz.tol=1e-5, batch.align.chr.tol=50, file.pattern=".cdf", known.table=NA, n.nodes=4, min.pres=0.5, min.run=12, mz.tol=1e-5, baseline.correct.noise.percentile=0.05, shape.model="bi-Gaussian",  baseline.correct=0, peak.estim.method="moment", min.bw=NA, max.bw=NA, sd.cut=c(0.1,100), sigma.ratio.lim=c(0.05, 20), component.eliminate=0.01, moment.power=2, align.mz.tol=NA, align.chr.tol=NA, max.align.mz.diff=0.01, pre.process=FALSE, recover.mz.range=NA, recover.chr.range=NA, use.observed.range=TRUE, match.tol.ppm=NA, new.feature.min.count=2, recover.min.count=3)
{
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(folder)
    info<-as.matrix(as.data.frame(info))
    batches<-unique(info[,2])
    
    batchwise<-new("list")
    message("total number of batches: ", length(batches))
    
    for(batch.i in 1:length(batches))
    {
        message("working on batch number ", batch.i)
        batch<-batches[batch.i]
        files<-dir(path = ".", pattern=file.pattern, ignore.case = TRUE)
        this.subs<-which(files %in% info[info[,2]==batch,1])
		message("total number of files in this batch ", length(this.subs))

        b<-semi.sup(folder, folder, n.nodes=n.nodes, subs=this.subs, file.pattern=file.pattern,known.table=known.table, sd.cut=sd.cut,sigma.ratio.lim=sigma.ratio.lim, component.eliminate=component.eliminate, moment.power=moment.power, min.pres=min.pres, min.run=min.run, min.exp=ceiling(min.within.batch.prop.detect*length(this.subs)), mz.tol=mz.tol, baseline.correct.noise.percentile=baseline.correct.noise.percentile, baseline.correct=baseline.correct, align.mz.tol=align.mz.tol, align.chr.tol=align.chr.tol, max.align.mz.diff=max.align.mz.diff, recover.mz.range=recover.mz.range, recover.chr.range=recover.chr.range,use.observed.range=use.observed.range, shape.model=shape.model,new.feature.min.count=new.feature.min.count, recover.min.count=recover.min.count)
        
        b$final.ftrs<-b$final.ftrs[order(b$final.ftrs[,1], b$final.ftrs[,2]),]
        b$final.times<-b$final.times[order(b$final.times[,1], b$final.times[,2]),]
        
        batchwise[[batch.i]]<-b
    }
    
    fake.features<-new("list")
    for(batch.i in 1:length(batches))
    {        
        this.fake<-batchwise[[batch.i]]$final.ftrs
        this.fake[,3:4]<-NA
        this.fake[,5]<-apply(this.fake[,-1:-4],1,median)
        this.fake<-this.fake[,1:5]
        fake.features[[batch.i]]<-this.fake   
    }
    
    cl <- makeCluster(n.nodes)
    registerDoParallel(cl)
    #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
    clusterEvalQ(cl, library(apLCMS))
    
    fake2<-adjust.time(fake.features,mz.tol=batch.align.mz.tol, chr.tol=batch.align.chr.tol, find.tol.max.d=10*mz.tol, max.align.mz.diff=max.align.mz.diff)
    
    message("Alignment across batches")
    fake3<-feature.align(fake2, min.exp=ceiling(min.batch.prop*length(batches)), mz.tol=batch.align.mz.tol,chr.tol=batch.align.chr.tol, find.tol.max.d=10*mz.tol, max.align.mz.diff=max.align.mz.diff)
    
    stopCluster(cl)

    message("Recovery across batches")
    
    cl <- makeCluster(n.nodes)
    registerDoParallel(cl)
    #clusterEvalQ(cl, source("~/Desktop/Dropbox/1-work/apLCMS_code/new_proc_cdf.r"))
    clusterEvalQ(cl, library(apLCMS))

    #aligned<-foreach(batch.i=1:length(batches), .combine=cbind) %dopar%
	for(batch.i in 1:length(batches))
    {
        this.fake<-batchwise[[batch.i]]$final.ftrs
		this.fake.time<-batchwise[[batch.i]]$final.times
        this.features<-batchwise[[batch.i]]$features2
        this.medians<-apply(this.fake[,-1:-4],1,median)
        
        orig.time<-this.fake[,2]
        adjusted.time<-fake2[[batch.i]][,2]
        
        this.pk.time<-this.aligned<-matrix(0, nrow=nrow(fake3$aligned.ftrs), ncol=ncol(this.fake)-4)
        
        # adjusting the time (already within batch adjusted)
        
        for(j in 1:length(this.features))
        {
            for(i in 1:nrow(this.features[[j]]))
            {
                diff.time<- abs(orig.time - this.features[[j]][i,2])
                sel<-which(diff.time == min(diff.time))[1]
                this.features[[j]][i,2] <- adjusted.time[sel]
            }
        }
        
        for(i in 1:nrow(this.aligned))
        {
            if(fake3$aligned[i, batch.i+4] != 0)
            {
                sel<-which(fake3$aligned[i,3]<=this.fake[,1] & fake3$aligned[i,4]>=this.fake[,1] & abs(this.medians - fake3$aligned[i, batch.i+4]) < 1)
                if(length(sel) == 0) sel<-which(fake3$aligned[i,3]<=this.fake[,1] & fake3$aligned[i,4]>=this.fake[,1])
                if(length(sel) == 0)
                {
                    message("batch", batch.i, " row ", i, " match issue.")
                }else{
                    if(length(sel) == 1)
                    {
                        this.aligned[i, ]<-this.fake[sel,-1:-4]
						this.pk.time[i, ]<-this.fake.time[sel,-1:-4]
                    }else{
                        this.aligned[i, ]<-apply(this.fake[sel,-1:-4],2,sum)
						this.pk.time[i, ]<-apply(this.fake.time[sel,-1:-4],2,median)
                    }
                }
            }else{
                ### go into individual feature tables to find a match
                recaptured<-rep(0, ncol(this.aligned))
				recaptured.time<-rep(NA, ncol(this.aligned))
                for(j in 1:length(this.features))
                {
                    diff.mz<-abs(this.features[[j]][,1]-fake3$aligned[i,1])
                    diff.time<-abs(this.features[[j]][,2]-fake3$aligned[i,2])
                    sel<-which(diff.mz < fake3$aligned[i,1]*batch.align.mz.tol & diff.time <= batch.align.chr.tol)
                    if(length(sel)>1) sel<-sel[which(diff.time[sel] == min(diff.time[sel]))[1]]
                    
                    if(length(sel) ==1) 
					{
						recaptured[j]<-this.features[[j]][sel,5]
						recaptured.time[j]<-this.features[[j]][sel,2]
					}
                }
                this.aligned[i, ]<-recaptured
				this.pk.time[i, ]<-recaptured.time
            }
        }
        
        colnames(this.aligned)<-colnames(this.fake)[-1:-4]
		colnames(this.pk.time)<-colnames(this.fake)[-1:-4]
        
		#### go back to cdf files
		
		that.aligned<-cbind(fake3$aligned.ftrs[,1:4],this.aligned)
		that.pk.time<-cbind(fake3$aligned.ftrs[,1:4],this.pk.time)
		
		#for(i in 1:ncol(this.aligned))
		new.this.aligned<-foreach(i=1:ncol(this.aligned), .combine=cbind) %dopar%
		{
			r<-recover.weaker(filename=colnames(this.aligned)[i], loc=i, aligned.ftrs=that.aligned, pk.times=that.pk.time, align.mz.tol=batch.align.mz.tol, align.chr.tol=batch.align.chr.tol, this.f1=batchwise[[batch.i]]$features[[i]], this.f2=batchwise[[batch.i]]$features2[[i]], mz.range=recover.mz.range, chr.range=recover.chr.range, use.observed.range=use.observed.range, orig.tol=mz.tol, min.bw=min.bw, max.bw=max.bw, bandwidth=.5, recover.min.count=recover.min.count)
			
			r$this.ftrs
		}
		
		colnames(new.this.aligned)<-colnames(this.aligned)
		
		####
		
		if(batch.i == 1)
		{
			aligned<-new.this.aligned
		}else{
			aligned<-cbind(aligned, new.this.aligned)
		}
    }
    
    aligned<-cbind(fake3$aligned.ftrs[,1:4], aligned)
    
	batch.presence.mat<-matrix(0, nrow=nrow(aligned), ncol=length(batches))
	for(batch.i in 1:length(batches))
	{
		batch<-batches[batch.i]
        this.mat<-aligned[,which(colnames(aligned) %in% info[info[,2]==batch,1])]
		this.mat<- 1*(this.mat != 0)
		this.presence<-apply(this.mat,1,sum)/ncol(this.mat)
		batch.presence.mat[,batch.i]<-1*(this.presence >= min.within.batch.prop.report)
	}
	batch.presence<-apply(batch.presence.mat,1,sum)/ncol(batch.presence.mat)
	final.aligned<-aligned[which(batch.presence >= min.batch.prop),]
	
    stopCluster(cl)
    to.return<-new("list")
    to.return$batchwise.results<-batchwise
	to.return$all.detected.ftrs<-aligned
    to.return$final.ftrs<-final.aligned
    return(to.return)
}
