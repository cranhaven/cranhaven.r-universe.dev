EIC.plot.learn <-
function(aligned, rows=NA, colors=NA, transform="none", subset=NA, tol=2.5e-5, ridge.smoother.window=50, baseline.correct=0, max.spline.time.points=1000)  # no object named "raw.prof" should exist. Otherwise it will be overwritten
{
    if(!(transform %in% c("none","log","sqrt","cuberoot")))
    {
        stop("Invalid transformation. It has to be from: none, log, sqrt, and cuberoot")
    }
    if(!is.na(rows[1]))
    {
        num.exp<-nrow(summary(aligned$features))
        if(is.na(subset[1])) subset<-1:num.exp
        if(is.na(colors[1])) colors<-c("red","blue","dark blue","orange","green","yellow","cyan","pink","violet","bisque","azure","brown","chocolate",rep("grey",length(subset)))
        files<-colnames(aligned$final.ftrs)[5:(num.exp+4)]
        rawprof.names<-paste(unlist(strsplit(tolower(files),"\\."))[seq(1, 2*num.exp,by=2)], "_", tol,"_",ridge.smoother.window, "_",baseline.correct, ".rawlearn",sep="")
        
        adj.times<-new("list")
        for(i in subset)
        {
            load(rawprof.names[i])
            times<-unique(raw.prof$labels)
            times<-times[order(times)]
            
            orig.time<-aligned$features[[i]][!is.na(aligned$features[[i]][,3]),2]
            adjusted.time<-aligned$features2[[i]][!is.na(aligned$features2[[i]][,3]),2]
            orig.time<-round(orig.time,8)
            adjusted.time<-round(adjusted.time,8)
            ttt<-table(adjusted.time)
            ttt2<-table(orig.time)
            to.use<-which(adjusted.time %in% as.numeric(names(ttt)[ttt==1]) & orig.time %in% as.numeric(names(ttt2)[ttt2==1]))
            if(length(to.use) > max.spline.time.points) to.use<-sample(to.use, max.spline.time.points, replace=FALSE)
            orig.time<-orig.time[to.use]
            adjusted.time<-adjusted.time[to.use]
            
            sp<-interpSpline(adjusted.time~orig.time)
            adj.times[[i]]<-predict(sp,times)$y
        }
        for(n in 1:length(rows))
        {
            this.row<-rows[n]
            this.intensi<-aligned$final.ftrs[this.row,5:(num.exp+4)]
            this.times<-aligned$final.times[this.row,5:(num.exp+4)]
            this.mz<-aligned$final.ftrs[this.row,1]
            to.plot<-new("list")
            y.max<-0
            x.max<-0
            x.min<-Inf
            
            for(iii in 1:length(subset))
            {
                i<-subset[iii]
                if(this.intensi[i] != 0)
                {
                    load(rawprof.names[i])
                    times<-unique(raw.prof$labels)
                    times<-times[order(times)]
                    
                    mz.diff<-abs(aligned$features2[[i]][,1]-this.mz)
                    time.diff<-abs(aligned$features2[[i]][,2]-this.times[i])
                    sel<-which(time.diff < 1e-17)
                    if(length(sel)>1) sel<-sel[which(mz.diff[sel]==min(mz.diff[sel]))[1]]
                    if(length(sel)==0) # this means two peaks are merged at the alignment step
                    # they share labels in the aligned$features2 object
                    {
                        mz.lim<-aligned$final.ftrs[this.row,c(3,4)]
                        sel<-which(aligned$features2[[i]][,1] >= mz.lim[1] & aligned$features2[[i]][,1] <= mz.lim[2] & !is.na(aligned$features2[[i]][,6]))
                        sub.features<-aligned$features2[[i]][sel,]
                        sub.time.diff<-abs(sub.features[,2]-this.times[i])
                        sel<-sel[which(sub.time.diff == min(sub.time.diff))][1]
                    }
                    
                    target.mz<-aligned$features2[[i]][sel,1]
                    target.time<-aligned$features[[i]][sel,2]
                    time.adjust<-aligned$features2[[i]][sel,2]-aligned$features[[i]][sel,2]
                    
                    mz.diff<-abs(raw.prof$masses - target.mz)
                    sel.slice<-raw.prof$grps[which(mz.diff==min(mz.diff))[1]]
                    sel.time.range<-range(raw.prof$labels[raw.prof$grps == sel.slice])
                    
                    while(target.time < sel.time.range[1] | target.time > sel.time.range[2])
                    {
                        mz.diff[raw.prof$grps == sel.slice]<-100
                        sel.slice<-raw.prof$grps[which(mz.diff==min(mz.diff))[1]]
                        sel.time.range<-range(raw.prof$labels[raw.prof$grps == sel.slice])
                    }
                    
                    sel.time<-raw.prof$labels[raw.prof$grps == sel.slice]
                    sel.intensi<-raw.prof$intensi[raw.prof$grps == sel.slice]
                    sel.intensi<-sel.intensi[order(sel.time)]
                    sel.time<-sel.time[order(sel.time)]
                    
                    all.intensi<-times*0
                    all.intensi[times %in% sel.time]<-sel.intensi
                    all.times<-adj.times[[i]]
                    
                    if(transform=="log") all.intensi<-log10(all.intensi+1)
                    if(transform=="sqrt") all.intensi<-sqrt(all.intensi)
                    if(transform=="cuberoot") all.intensi<-all.intensi^(1/3)
                    
                    if(max(all.intensi) > y.max) y.max<-max(all.intensi)
                    if(max(all.times[all.intensi>0]) > x.max) x.max<-max(all.times[all.intensi>0])
                    if(min(all.times[all.intensi>0]) < x.min) x.min<-min(all.times[all.intensi>0])
                    to.plot[[i]]<-cbind(all.times, all.intensi)
                }
            }
            
            for(iii in 1:min(length(subset), nrow(summary(to.plot))))
            {
                i<-subset[iii]
                if(iii==1)
                {
                    plot(to.plot[[i]], xlim=c(x.min, x.max),ylim=c(0,y.max),type="l",col=colors[iii],xlab="retention time",ylab="intensity",main=paste("EIC for row",rows[n],",m/z",round(this.mz,4)))
                }else{
                    lines(to.plot[[i]], col=colors[iii])
                }
                #				abline(v=this.times[i],col=colors[i])
            }
        }
        message("the colors used are:")
        message(paste(subset, ":", colors[1:length(subset)], sep = "", collapse = " "))
    }
}
