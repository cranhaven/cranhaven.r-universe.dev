feature.align <-
function(features, min.exp=2,mz.tol=NA,chr.tol=NA,find.tol.max.d=1e-4, max.align.mz.diff=0.01)     # returns a list of aligned features and original peak times
{
    oldpar <- par(no.readonly = TRUE)   
    on.exit(par(oldpar))
    par(mfrow=c(3,2))
    plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="",axes=FALSE)
    text(x=0,y=0,"Feature alignment",cex=2)
    plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="",axes=FALSE)
    
    to.attach<-function(this.pick, num.exp, use="sum")
    {
        this.strengths<-rep(0, num.exp)
        if(is.null(nrow(this.pick)))
        {
            this.strengths[this.pick[6]]<-this.pick[5]
            return(c(this.pick[1], this.pick[2], this.pick[1],this.pick[1],this.strengths))
        }else{
            for(m in 1:length(this.strengths))
            {
                if(use == "sum") this.strengths[m]<-sum(this.pick[this.pick[,6]==m,5])
                if(use == "median") this.strengths[m]<-median(this.pick[this.pick[,6]==m,5])
            }
            return(c(mean(this.pick[,1]),mean(this.pick[,2]),min(this.pick[,1]),max(this.pick[,1]), this.strengths))
        }
    }
    
    num.exp<-nrow(summary(features))
    if(num.exp>1)
    {
        a<-summary(features)
        sizes<-as.numeric(a[,1])/ncol(features[[1]])
        sizes<-cumsum(sizes)
        sel<-length(sizes)
        
        masses<-chr<-lab<-rep(0, sizes[sel])
        sizes<-c(0, sizes)
        
        for(i in 1:sel)
        {
            masses[(sizes[i]+1):sizes[i+1]]<-features[[i]][,1]
            chr[(sizes[i]+1):sizes[i+1]]<-features[[i]][,2]
            lab[(sizes[i]+1):sizes[i+1]]<-i
        }
        o<-order(masses, chr)
        masses<-masses[o]
        chr<-chr[o]
        lab<-lab[o]
        l<-length(masses)
        
        if(is.na(mz.tol))
        {
            mz.tol<-find.tol(masses,uppermost=find.tol.max.d)
            if(length(mz.tol)==0)
            {
                mz.tol<-1e-5
                warning("Automatic tolerance finding failed, 10 ppm was assigned. May need to manually assign alignment mz tolerance level.")
            }
        }else{
            plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="alignment m/z tolerance level given",axes=FALSE)
            text(x=0,y=0,mz.tol,cex=1.2)
        }
        
        if(!is.na(chr.tol))
        {
            plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="retention time \n tolerance level given",axes=FALSE)
            text(x=0,y=0,chr.tol,cex=1.2)
        }
        
        all.ft<-find.tol.time(masses, chr, lab, num.exp=num.exp, mz.tol=mz.tol, chr.tol=chr.tol, max.mz.diff=max.align.mz.diff)
        chr.tol<-all.ft$chr.tol
        
        message("**** performing feature alignment ****")
        message(paste("m/z tolerance level: ", mz.tol))
        message(paste("time tolerance level:", chr.tol))
        
        aligned.ftrs<-pk.times<-rep(0,4+num.exp)
        mz.sd.rec<-0
        
        labels<-unique(all.ft$grps)
        
        area<-grps<-masses
        
        
        for(i in 1:num.exp)
        {
            this<-features[[i]]
            sel<-which(all.ft$lab == i)
            that<-cbind(all.ft$mz[sel], all.ft$chr[sel], all.ft$grps[sel])
            this<-this[order(this[,1], this[,2]), ]
            that<-that[order(that[,1], that[,2]), ]
            
            masses[(sizes[i]+1):sizes[i+1]]<-this[,1]
            chr[(sizes[i]+1):sizes[i+1]]<-this[,2]
            area[(sizes[i]+1):sizes[i+1]]<-this[,5]
            grps[(sizes[i]+1):sizes[i+1]]<-that[,3]
            lab[(sizes[i]+1):sizes[i+1]]<-i
        }
        
        ttt<-table(all.ft$grps)
        curr.row<-sum(ttt>=min.exp)*3
        mz.sd.rec<-rep(0, curr.row)
        curr.row<-1
        
        sel.labels<-as.numeric(names(ttt)[ttt>=min.exp])
        
        aligned.ftrs<-foreach(i=1:length(sel.labels), .combine=rbind) %dopar%
        {
            if(i %% 100 == 0) gc()
            this.return<-NULL
            sel<-which(grps==sel.labels[i])
            
            #			print(c(i, sel.labels[i], length(sel), length(unique(sel)), curr.row))
            
            if(length(sel)>1)
            {
                this<-cbind(masses[sel], chr[sel], chr[sel], chr[sel], area[sel],lab[sel])
                if(length(unique(this[,6])) >= min.exp)
                {
                    this.den<-density(this[,1], bw=mz.tol*median(this[,1]))
                    turns<-find.turn.point(this.den$y)
                    pks<-this.den$x[turns$pks]
                    vlys<-this.den$x[turns$vlys]
                    for(j in 1:length(pks))
                    {
                        this.lower<-max(vlys[vlys < pks[j]])
                        this.upper<-min(vlys[vlys > pks[j]])
                        this.sel<-which(this[,1] > this.lower & this[,1] <= this.upper)
                        that<-this[this.sel,]
                        if(!is.null(nrow(that)))
                        {
                            if(length(unique(that[,6])) >= min.exp)
                            {
                                that.den<-density(that[,2], bw=chr.tol/1.414)
                                that.turns<-find.turn.point(that.den$y)
                                that.pks<-that.den$x[that.turns$pks]
                                that.vlys<-that.den$x[that.turns$vlys]
                                for(k in 1:length(that.pks))
                                {
                                    that.lower<-max(that.vlys[that.vlys < that.pks[k]])
                                    that.upper<-min(that.vlys[that.vlys > that.pks[k]])
                                    
                                    thee<-that[that[,2] > that.lower & that[,2] <= that.upper,]
                                    if(!is.null(nrow(thee)))
                                    {
                                        if(length(unique(thee[,6])) >= min.exp)
                                        {
                                            this.return<-c(to.attach(thee, num.exp, use="sum"),to.attach(thee[,c(1,2,3,4,2,6)], num.exp, use="median"), sd(thee[,1],na.rm=TRUE))
                                        }
                                    }
                                }
                            }else{}
                        }
                    }
                }else{}
            }else{
                if(min.exp==1)
                {
                    thee<-c(masses[sel], chr[sel], chr[sel], chr[sel], area[sel],lab[sel])
                    this.return<-c(to.attach(thee, num.exp, use="sum"), to.attach(thee[c(1,2,3,4,2,6)], num.exp, use="median"), NA)
                }
            }
            this.return
        }
        
        pk.times<-aligned.ftrs[,(5+num.exp):(2*(4+num.exp))]
        mz.sd.rec<-aligned.ftrs[,ncol(aligned.ftrs)]
        aligned.ftrs<-aligned.ftrs[,1:(4+num.exp)]
        
        colnames(aligned.ftrs)<-c("mz","chr","min.mz","max.mz",paste("exp",1:num.exp))
        colnames(pk.times)<-c("mz","chr","min.mz","max.mz",paste("exp",1:num.exp))
        
        rec<-new("list")
        rec$aligned.ftrs<-aligned.ftrs
        rec$pk.times<-pk.times
        rec$mz.tol<-mz.tol
        rec$chr.tol<-chr.tol
        
        hist(mz.sd.rec,xlab="m/z SD", ylab="Frequency",main="m/z SD distribution")
        hist(apply(pk.times[,-1:-4],1,sd,na.rm=TRUE), xlab="Retention time SD", ylab="Frequency", main="Retention time SD distribution")
        
        return(rec)
    }else{
        message("There is but one experiment.  What are you trying to align?")
        return(0)
    }
}
