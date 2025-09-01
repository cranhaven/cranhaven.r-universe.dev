prof.to.features <-
function(a, bandwidth=0.5, min.bw=NA, max.bw=NA, sd.cut=c(0.1,100), sigma.ratio.lim=c(0.1, 10), shape.model="bi-Gaussian", estim.method="moment",do.plot=TRUE, power=1, component.eliminate=0.01, BIC.factor=2)
{
    if(sum(shape.model %in% c("bi-Gaussian", "Gaussian")) == 0)
    {
        message("Error: peak shape model has to be Gaussian or bi-Gaussian")
        return(0)
    }
    if(sum(estim.method %in% c("moment", "EM")) == 0)
    {
        message("Error: peak model estimation method has to be moment or EM")
        return(0)
    }
    
    #####################
    bigauss.esti.EM <- function(t, x, max.iter=50, epsilon=0.005, power=1, do.plot=FALSE, truth=NA, sigma.ratio.lim=c(0.3, 1))
    {
        
        ## function takes into x and t, and then computes the value of
        ## sigma.1, sigma.2 and a using iterative method. the returned
        ## values include estimated sigmas, a and a boolean variable on
        ## whether the termination criteria is satified upon the end of the
        ## program.
        
        sel<-which(x>1e-10)
        if(length(sel)==0)
        {
            return(c(median(t),1,1,0))
        }
        if(length(sel)==1)
        {
            return(c(t[sel], 1,1,0))
        }
        t<-t[sel]
        x<-x[sel]
        solve.sigma <- function(x,t,a){
            
            ## this function takes the value intensity level x, retention time t
            ## and assumed breaking point a, calculates the square estimated of
            ## sigma.1 and sigma.2.
            
            prep.uv <- function(x,t,a){
                
                ## this function prepares the parameters required for latter
                ## compuation. u, v, and sum of x.
                
                temp <- (t-a)^2 * x
                u <- sum(temp * as.numeric(t<a))
                v <- sum(temp * as.numeric(t>=a))
                return(list(u=u,
                v=v,
                x.sum=sum(x)))
            }
            tt <- prep.uv(x,t,a)
            sigma.1 <- tt$u/tt$x.sum*((tt$v/tt$u)^(1/3)+1)
            sigma.2 <- tt$v/tt$x.sum*((tt$u/tt$v)^(1/3)+1)
            return(list(sigma.1=sigma.1,
            sigma.2=sigma.2))
        }
        
        solve.a <- function(x,t,a,sigma.1,sigma.2){
            
            ## thif function solves the value of a using the x, t, a from the
            ## previous step, and sigma.1, and sigma.2
            
            w <- x * (as.numeric(t<a)/sigma.1 + as.numeric(t>=a)/sigma.2)
            return(sum(t * w)/ sum(w))
        }
        
        ## epsilon is the threshold for continuing the iteration. change in
        ## a smaller than epsilon will terminate the iteration.
        ## epsilon <- min(diff(sort(t)))/2
        
        ## using the median value of t as the initial value of a.
        a.old <- t[which(x==max(x))[1]]
        a.new <- a.old
        change <- 10*epsilon
        
        ## n.iter is the number of iteration covered so far.
        n.iter <- 0
        
        while((change>epsilon) & (n.iter<max.iter)){
            ## message(paste(n.iter, change))
            a.old <- a.new
            n.iter <- n.iter+1
            sigma <- solve.sigma(x,t,a.old)
            if(n.iter == 1) sigma[is.na(sigma)]<-as.numeric(sigma[which(!is.na(sigma))])[1]/10
            a.new <- solve.a(x,t,a.old,sigma$sigma.1,sigma$sigma.2)
            change <- abs(a.old-a.new)
        }
        #  return(list(a=a.new,
        #              sigma.1=sigma$sigma.1,
        #              sigma.2=sigma$sigma.2,
        #              iter.end=(max.iter>n.iter)
        #              ))
        d<-x
        sigma$sigma.2<-sqrt(sigma$sigma.2)
        sigma$sigma.1<-sqrt(sigma$sigma.1)
        
        d[t<a.new]<-dnorm(t[t<a.new],mean=a.new,sd=sigma$sigma.1)*sigma$sigma.1
        d[t>=a.new]<-dnorm(t[t>=a.new],mean=a.new,sd=sigma$sigma.2)*sigma$sigma.2
        scale<-exp(sum(d[d>1e-3]^2*log(x[d>1e-3]/d[d>1e-3]))/sum(d[d>1e-3]^2))
        return(c(a.new, sigma$sigma.1, sigma$sigma.2, scale))
    }
    
    bigauss.esti<-function(x,y,power=1, do.plot=FALSE, truth=NA, sigma.ratio.lim=c(0.3, 3))
    {
        sel<-which(y>1e-10)
        if(length(sel)<2)
        {
            to.return<-c(median(x),1,1,0)
        }else{
            x<-x[sel]
            y<-y[sel]
            #			sel<-order(x)
            #			y<-y[sel]
            #			x<-x[sel]
            
            y.0<-y
            if(do.plot) plot(x,y)
            if(do.plot & ! is.na(truth[1]))
            {
                true.y1<-dnorm(x[x<truth[1]], mean=truth[1],sd=truth[2])*truth[2]*truth[4]
                true.y2<-dnorm(x[x>=truth[1]],mean=truth[1],sd=truth[3])*truth[3]*truth[4]
                lines(x, c(true.y1,true.y2),col="green")
            }
            max.y.0<-max(y.0,na.rm=TRUE)
            y<-(y/max.y.0)^power
            
            l<-length(x)
            min.d<-min(diff(x))
            dx<-c(x[2]-x[1], (x[3:l]-x[1:(l-2)])/2, x[l]-x[l-1])
            if(l ==2) dx=rep(diff(x),2)
            dx[dx>4*min.d]<-4*min.d
            
            y.cum<-cumsum(y*dx)
            x.y.cum<-cumsum(y*x*dx)
            xsqr.y.cum<-cumsum(y*x^2*dx)
            
            y.cum.rev<-cumsum((y*dx)[l:1])[l:1]    # reverse cum sum
            x.y.cum.rev<-cumsum((x*y*dx)[l:1])[l:1]
            xsqr.y.cum.rev<-cumsum((y*x^2*dx)[l:1])[l:1]
            
            sel<-which(y.cum >= sigma.ratio.lim[1]/(sigma.ratio.lim[1] + 1)*y.cum[l])
            if(length(sel)>0)
            {
                start<-max(1, min(sel))
            }else{
                start<-1
            }
            sel<-which(y.cum <= sigma.ratio.lim[2]/(sigma.ratio.lim[2] + 1)*y.cum[l])
            if(length(sel)>0)
            {
                end<-min(l-1, max(sel))
            }else{
                end<-l-1
            }
            if(end <= start)
            {
                m<-min(mean(x[start:end]), x[max(which(y.cum.rev>0))])
            }else{
                m.candi<-x[start:end]+diff(x[start:(end+1)])/2
                rec<-matrix(0, ncol=3, nrow=end-start+1)
                
                s1<-sqrt((xsqr.y.cum[start:end]+m.candi^2*y.cum[start:end]-2*m.candi*x.y.cum[start:end])/y.cum[start:end])
                s2<-sqrt((xsqr.y.cum.rev[start:end+1]+m.candi^2*y.cum.rev[start:end+1]-2*m.candi*x.y.cum.rev[start:end+1])/y.cum.rev[start:end+1])
                rec[,1]<-s1
                rec[,2]<-s2
                rec[,3]<-y.cum[start:end]/y.cum.rev[start:end+1]
                
                d<-log(rec[,1]/rec[,2])-log(rec[,3])
                if(min(d,na.rm=TRUE)*max(d,na.rm=TRUE) < 0)
                {
                    sel<-c(which(d==max(d[d<0]))[1], which(d==min(d[d>=0])))
                    m<-(sum(abs(d[sel])*m.candi[sel]))/(sum(abs(d[sel])))
                }else{
                    d<-abs(d)
                    m<-m.candi[which(d==min(d,na.rm=TRUE))[1]]
                }
            }
            
            if(do.plot) abline(v=m)
            
            sel1<-which(x<m)
            sel2<-which(x>=m)
            s1<-sqrt(sum((x[sel1]-m)^2*y[sel1]*dx[sel1])/sum(y[sel1]*dx[sel1]))
            s2<-sqrt(sum((x[sel2]-m)^2*y[sel2]*dx[sel2])/sum(y[sel2]*dx[sel2]))
            
            
            if(power != 1)
            {
                s1<-s1*sqrt(power)
                s2<-s2*sqrt(power)
            }
            
            d1<-dnorm(x[sel1], sd=s1, mean=m)
            d2<-dnorm(x[sel2], sd=s2, mean=m)
            d<-c(d1*s1,d2*s2)                            # notice this "density" doesnt integrate to 1. Rather it integrates to (s1+s2)/2
            y<-y.0
            
            dy.ratio<-d^2*log(y/d)
            dy.ratio[is.na(dy.ratio)]<-0
            dy.ratio[dy.ratio == -Inf]<-0
            dy.ratio[dy.ratio == Inf]<-0
            
            
            scale<-exp(sum(dy.ratio)/sum(d^2))
            
            if(do.plot)
            {
                fit.1<-d*scale
                lines(x[y>0],fit.1,col="red")
            }
            
            to.return<-c(m,s1,s2,scale)
            if(sum(is.na(to.return)) > 0)
            {
                m<-sum(x*y)/sum(y)
                s1<-s2<-sum(y*(x-m)^2)/sum(y)
                scale<-sum(y)/s1
                to.return<-c(m,s1,s2,scale)
            }
        }
        return(to.return)
    }
    
    ##############
    bigauss.mix<-function(x,y,power=1, do.plot=FALSE, sigma.ratio.lim=c(0.1, 10), bw=c(15,30,60), eliminate=.05, max.iter=25, estim.method, BIC.factor=2)
    {
        all.bw<-bw[order(bw)]
        
        x.0<-x
        y.0<-y
        
        sel<-y>1e-5
        x<-x[sel]
        y<-y[sel]
        sel<-order(x)
        y<-y[sel]
        x<-x[sel]
        results<-new("list")
        smoother.pk.rec<-smoother.vly.rec<-new("list")
        bic.rec<-all.bw
        
        if(do.plot)
        {
            oldpar <- par(no.readonly = TRUE)
            on.exit(par(oldpar))
            par(mfrow=c(ceiling(length(all.bw)/2),2))
            par(mar=c(1,1,1,1))
        }
        
        last.num.pks<-Inf
        
        for(bw.n in length(all.bw):1)
        {
            bw<-all.bw[bw.n]
            this.smooth<-ksmooth(x.0,y.0, kernel="normal", bandwidth=bw)
            turns<-find.turn.point(this.smooth$y)
            pks<-this.smooth$x[turns$pks]
            vlys<-c(-Inf, this.smooth$x[turns$vlys], Inf)
            
            smoother.pk.rec[[bw.n]]<-pks
            smoother.vly.rec[[bw.n]]<-vlys
            if(length(pks) != last.num.pks)
            {
                last.num.pks<-length(pks)
                l<-length(x)
                dx<-c(x[2]-x[1], (x[3:l]-x[1:(l-2)])/2, x[l]-x[l-1])
                if(l ==2) dx=rep(diff(x),2)
                
                # initiation
                m<-s1<-s2<-delta<-pks
                for(i in 1:length(m))
                {
                    sel.1<-which(x >= max(vlys[vlys < m[i]]) & x < m[i])
                    s1[i]<-sqrt(sum((x[sel.1]-m[i])^2 * y[sel.1]*dx[sel.1])/sum(y[sel.1]*dx[sel.1]))
                    
                    sel.2<-which(x >= m[i] & x < min(vlys[vlys > m[i]]))
                    s2[i]<-sqrt(sum((x[sel.2]-m[i])^2 * y[sel.2] * dx[sel.2])/sum(y[sel.2]*dx[sel.2]))
                    
                    delta[i]<-(sum(y[sel.1]*dx[sel.1]) + sum(y[sel.2]*dx[sel.2]))/((sum(dnorm(x[sel.1], mean=m[i], sd=s1[i])) * s1[i] /2)+(sum(dnorm(x[sel.2], mean=m[i], sd=s2[i])) * s2[i] /2))
                }
                delta[is.na(delta)]<-1e-10
                s1[is.na(s1)]<-1e-10
                s2[is.na(s2)]<-1e-10
                
                
                fit<-matrix(0,ncol=length(m), nrow=length(x))   # this is the matrix of fitted values
                
                this.change=Inf
                counter=0
                
                while(this.change > 0.1 & counter <= max.iter)
                {
                    counter<-counter+1
                    old.m<-m
                    
                    # E step
                    cuts<-c(-Inf, m, Inf)
                    for(j in 2:length(cuts))
                    {
                        sel<-which(x >= cuts[j-1] & x < cuts[j])
                        use.s1<-which(1:length(m) >= (j-1))
                        s.to.use<-s2
                        s.to.use[use.s1]<-s1[use.s1]
                        for(i in 1:ncol(fit))
                        {
                            fit[sel,i]<-dnorm(x[sel], mean=m[i], sd = s.to.use[i]) * s.to.use[i] *delta[i]
                        }
                    }
                    fit[is.na(fit)]<-0
                    sum.fit<-apply(fit, 1, sum)
                    
                    # Elimination step
                    fit<-fit/sum.fit
                    fit2<-fit * y
                    perc.explained<-apply(fit2,2,sum)/sum(y)
                    max.erase<-max(1, round(length(perc.explained)/5))
                    
                    to.erase<-which(perc.explained <= min(eliminate, perc.explained[order(perc.explained, na.last=FALSE)[max.erase]]))
                    
                    
                    if(length(to.erase) > 0)
                    {
                        m<-m[-to.erase]
                        s1<-s1[-to.erase]
                        s2<-s2[-to.erase]
                        delta<-delta[-to.erase]
                        fit<-fit[,-to.erase]
                        if(is.null(ncol(fit))) fit<-matrix(fit, ncol=1)
                        sum.fit<-apply(fit, 1, sum)
                        fit<-fit/sum.fit
                        old.m<-old.m[-to.erase]
                    }
                    
                    # M step
                    for(i in 1:length(m))
                    {
                        this.y<-y * fit[,i]
                        if(estim.method=="moment")
                        {
                            this.fit<-bigauss.esti(x,this.y,power=power,do.plot=FALSE, sigma.ratio.lim=sigma.ratio.lim)
                        }else{
                            this.fit<-bigauss.esti.EM(x,this.y,power=power,do.plot=FALSE, sigma.ratio.lim=sigma.ratio.lim)
                        }
                        m[i]<-this.fit[1]
                        s1[i]<-this.fit[2]
                        s2[i]<-this.fit[3]
                        delta[i]<-this.fit[4]
                    }
                    delta[is.na(delta)]<-0
                    
                    #amount of change
                    this.change<-sum((old.m-m)^2)
                }
                cuts<-c(-Inf, m, Inf)
                fit<-fit*0
                for(j in 2:length(cuts))
                {
                    sel<-which(x >= cuts[j-1] & x < cuts[j])
                    use.s1<-which(1:length(m) >= (j-1))
                    s.to.use<-s2
                    s.to.use[use.s1]<-s1[use.s1]
                    for(i in 1:ncol(fit))
                    {
                        if(s.to.use[i] != 0)
                        {
                            fit[sel,i]<-dnorm(x[sel], mean=m[i], sd = s.to.use[i]) * s.to.use[i] *delta[i]
                        }
                    }
                }
                
                if(do.plot)
                {
                    plot(x,y,cex=.1,main=paste("bw=",bw))
                    sum.fit<-apply(fit, 1, sum)
                    lines(x,sum.fit)
                    abline(v=m)
                    cols<-c("red","green","blue","cyan","brown","black",rep("grey",100))
                    for(i in 1:length(m))
                    {
                        lines(x, fit[,i],col=cols[i])
                    }
                }
                area<-delta*(s1+s2)/2
                rss<-sum((y-apply(fit,1,sum))^2)
                l<-length(x)
                bic<-l*log(rss/l)+4*length(m)*log(l)*BIC.factor
                results[[bw.n]]<-cbind(m,s1,s2,delta,area)
                bic.rec[bw.n]<-bic
            }else{
                results[[bw.n]]<-NA
                bic.rec[bw.n]<-Inf
                results[[bw.n]]<-results[[bw.n+1]]
                
            }
        }
        sel<-which(bic.rec == min(bic.rec, na.rm=TRUE))
        if(length(sel) > 1) sel<-sel[which(all.bw[sel]==max(all.bw[sel]))]
        rec<-new("list")
        rec$param<-results[[sel]]
        rec$smoother.pks<-smoother.pk.rec
        rec$smoother.vlys<-smoother.vly.rec
        rec$all.param<-results
        rec$bic<-bic.rec
        return(rec)
    }
    
    #############
    normix<-function(that.curve, pks, vlys, ignore=0.1, max.iter=50, prob.cut=1e-2)
    {
        x<-that.curve[,1]
        y<-that.curve[,2]
        
        if(length(pks)==1)
        {
            miu<-sum(x*y)/sum(y)
            sigma<-sqrt(sum(y*(x-miu)^2)/sum(y))
            fitted<-dnorm(x, mean=miu, sd=sigma)
            this.sel<-y>0 & fitted/dnorm(miu,mean=miu,sd=sigma)>prob.cut
            sc<-exp(sum(fitted[this.sel]^2*log(y[this.sel]/fitted[this.sel])/sum(fitted[this.sel]^2)))
        }else{
            pks<-pks[order(pks)]
            vlys<-vlys[order(vlys)]
            l<-length(pks)
            miu<-sigma<-sc<-pks
            w<-matrix(0,nrow=l, ncol=length(x))
            
            for(m in 1:l)
            {
                this.low<-max(vlys[vlys<=pks[m]])
                this.high<-min(vlys[vlys>=pks[m]])
                this.x<-x[x>=this.low & x<=this.high]
                this.y<-y[x>=this.low & x<=this.high]
                
                miu[m]<-sum(this.x*this.y)/sum(this.y)
                #if(sum(this.y>0) > 1)
                #{
                sigma[m]<-sqrt(sum(this.y*(this.x-miu[m])^2)/sum(this.y))
                #}else{
                #  sigma[m]<-mean(diff(this.x))/2
                #}
                fitted<-dnorm(this.x, mean=miu[m], sd=sigma[m])
                this.sel<-this.y>0 & fitted/dnorm(miu[m],mean=miu[m],sd=sigma[m]) > prob.cut
                sc[m]<-exp(sum(fitted[this.sel]^2*log(this.y[this.sel]/fitted[this.sel])/sum(fitted[this.sel]^2)))
                #sc[m]<-lm(this.y[this.sel]~fitted[this.sel]+0)$coef
            }
            
            to.erase<-which(is.na(miu) | is.na(sigma) | sigma==0 | is.na(sc))
            if(length(to.erase)>0)
            {
                l<-l-length(to.erase)
                miu<-miu[-to.erase]
                sigma<-sigma[-to.erase]
                sc<-sc[-to.erase]
                w<-w[-to.erase,]
            }
            
            direc<-1
            diff<-1000
            iter<-0
            
            while(diff>0.05 & iter<max.iter)
            {
                iter<-iter+1
                if(l==1)
                {
                    miu<-sum(x*y)/sum(y)
                    sigma<-sqrt(sum(y*(x-miu)^2)/sum(y))
                    fitted<-dnorm(x, mean=miu, sd=sigma)
                    this.sel<-y>0 & fitted/dnorm(miu,mean=miu,sd=sigma) > prob.cut
                    sc<-exp(sum(fitted[this.sel]^2*log(y[this.sel]/fitted[this.sel])/sum(fitted[this.sel]^2)))
                    #sc<-lm(y[this.sel]~fitted[this.sel]+0)$coef
                    break;
                }
                miu.0<-miu
                sigma.0<-sigma
                sc.0<-sc
                
                all.w<-y*0
                for(m in 1:l)
                {
                    all.w<-all.w+dnorm(x,mean=miu[m],sd=sigma[m])*sc[m]
                }
                
                for(m in 1:l)
                {
                    w[m,]<-dnorm(x,mean=miu[m],sd=sigma[m])*sc[m]/all.w
                }
                
                if(sum(is.na(w)) >0) break;
                
                for(m in 1:l)
                {
                    this.y<-y*w[m,]
                    miu[m]<-sum(x*this.y)/sum(this.y)
                    sigma[m]<-sqrt(sum(this.y*(x-miu[m])^2)/sum(this.y))
                    fitted<-dnorm(x,mean=miu[m],sd=sigma[m])
                    this.sel<-this.y>0 & fitted/dnorm(miu[m],mean=miu[m],sd=sigma[m])> prob.cut
                    sc[m]<-exp(sum(fitted[this.sel]^2*log(this.y[this.sel]/fitted[this.sel])/sum(fitted[this.sel]^2)))
                    #sc[m]<-lm(this.y[this.sel]~fitted[this.sel]+0)$coef
                }
                diff<-sum((miu.0-miu)^2)
                
                www<-w
                for(m in 1:l)
                {
                    www[m,]<-www[m,]*y
                }
                www<-apply(www,1,sum)
                www[which(is.na(sc))]<-0
                www<-www/sum(www)
                max.erase<-max(1, round(l/5))
                
                to.erase<-which(www <= min(ignore, www[order(www, na.last=FALSE)[max.erase]]))
                
                if(length(to.erase)>0)
                {
                    l<-l-length(to.erase)
                    miu<-miu[-to.erase]
                    sigma<-sigma[-to.erase]
                    sc<-sc[-to.erase]
                    w<-w[-to.erase,]
                    diff<-1000
                }
            }
        }
        l<-length(miu)
        if(l==1)
        {
            rec<-matrix(c(miu, sigma, sc),nrow=1)
        }else{
            rec<-cbind(miu,sigma,sc)
        }
        colnames(rec)<-c("miu","sigma","scale")
        return(rec)
    }
    
    ##########
    
    normix.bic<-function(x,y,power=2, do.plot=FALSE, bw=c(15,30,60), eliminate=.05, max.iter=50, BIC.factor=2)
    {
        all.bw<-bw[order(bw)]
        sel<-y>1e-5
        x<-x[sel]
        y<-y[sel]
        sel<-order(x)
        y<-y[sel]
        x<-x[sel]
        results<-new("list")
        smoother.pk.rec<-smoother.vly.rec<-new("list")
        bic.rec<-all.bw
        
        if(do.plot)
        { 
            oldpar <- par(no.readonly = TRUE)
            on.exit(par(oldpar))
            par(mfrow=c(ceiling(length(all.bw)/2),2))
            par(mar=c(1,1,1,1))
        }
        
        last.num.pks<-Inf
        
        for(bw.n in length(all.bw):1)
        {
            bw<-all.bw[bw.n]
            this.smooth<-ksmooth(x,y, kernel="normal", bandwidth=bw)
            turns<-find.turn.point(this.smooth$y)
            pks<-this.smooth$x[turns$pks]
            vlys<-c(-Inf, this.smooth$x[turns$vlys], Inf)
            
            smoother.pk.rec[[bw.n]]<-pks
            smoother.vly.rec[[bw.n]]<-vlys
            if(length(pks) != last.num.pks)
            {
                last.num.pks<-length(pks)
                aaa<-normix(cbind(x,y), pks=pks, vlys=vlys, ignore=eliminate, max.iter=max.iter)
                
                total.fit<-x*0
                for(i in 1:nrow(aaa))
                {
                    total.fit<-total.fit+dnorm(x,mean=aaa[i,1], sd=aaa[i,2])*aaa[i,3]
                }
                
                if(do.plot)
                {
                    plot(x,y,cex=.1,main=paste("bw=",bw))
                    abline(v=aaa[,1])
                    cols<-c("red","green","blue","cyan","brown","black",rep("grey",100))
                    for(i in 1:nrow(aaa))
                    {
                        lines(x, dnorm(x,mean=aaa[i,1], sd=aaa[i,2])*aaa[i,3],col=cols[i])
                    }
                }
                
                rss<-sum((y-total.fit)^2)
                l<-length(x)
                bic<-l*log(rss/l)+3*nrow(aaa)*log(l)*BIC.factor
                results[[bw.n]]<-aaa
                bic.rec[bw.n]<-bic
            }else{
                bic.rec[bw.n]<-Inf
                results[[bw.n]]<-results[[bw.n+1]]
                
            }
        }
        sel<-which(bic.rec == min(bic.rec))
        if(length(sel) > 1) sel<-sel[which(all.bw[sel]==max(all.bw[sel]))]
        rec<-new("list")
        rec$param<-results[[sel]]
        rec$smoother.pks<-smoother.pk.rec
        rec$smoother.vlys<-smoother.vly.rec
        rec$all.param<-results
        rec$bic<-bic.rec
        return(rec)
    }
    
    ##########
    
    if(is.na(min.bw)) min.bw<-diff(range(a[,2], na.rm=TRUE))/60
    if(is.na(max.bw)) max.bw<-diff(range(a[,2], na.rm=TRUE))/15
    if(min.bw >= max.bw) min.bw<-max.bw/4
    
    base.curve<-unique(a[,2])
    all.span<-range(base.curve)
    base.curve<-base.curve[order(base.curve)]
    base.curve<-cbind(base.curve, base.curve*0)
    all.times<-base.curve[,1]
    if(all.times[1]>0) all.times<-c(0,all.times)
    all.times<-c(all.times, 2*all.times[length(all.times)]-all.times[length(all.times)-1])
    all.times<-(all.times[1:(length(all.times)-1)]+all.times[2:length(all.times)])/2
    all.times<-all.times[2:length(all.times)]-all.times[1:(length(all.times)-1)]
    
    this.features<-matrix(0,nrow=1,ncol=5)
    colnames(this.features)<-c("mz","pos","sd1","sd2","area")
    n=1
    start=1
    nrowa<-nrow(a)
    
    a.breaks<-c(0, which(a[1:(nrowa-1),4] != a[2:nrowa,4]), nrowa)
    mz.sd.rec<-NA
    
    for(nnn in 1:(length(a.breaks)-1))
    {
        this<-a[(a.breaks[nnn]+1):a.breaks[nnn+1],]
        if(is.null(nrow(this))) this<-matrix(this, nrow=1)
        this<-this[order(this[,2]),]
        if(is.null(nrow(this))) this<-matrix(this, nrow=1)
        mz.sd.rec<-c(mz.sd.rec, sd(this[,1]))
        
        this.count.1<-nrow(this)
        if(this.count.1<=10)
        {
            if(this.count.1>1)
            {
                this.inte<-interpol.area(this[,2], this[,3], base.curve[,1], all.times)
                xxx<-c(median(this[,1]),median(this[,2]), sd(this[,2]), sd(this[,2]), this.inte)
            }else{
                this.time.weights<-all.times[which(base.curve[,1] %in% this[2])]
                xxx<-c(this[1], this[2], NA, NA, this[3]*this.time.weights)
            }
            this.features<-rbind(this.features,xxx)
        }else{
            this.span<-range(this[,2])
            this.curve<-base.curve[base.curve[,1]>=this.span[1] & base.curve[,1] <=this.span[2],]
            this.curve[this.curve[,1] %in% this[,2],2]<-this[,3]
            
            bw<-min(max(bandwidth*(this.span[2]-this.span[1]),min.bw), max.bw)
            bw<-seq(bw, 2*bw, length.out=3)
            if(bw[1] > 1.5*min.bw) bw<-c(max(min.bw, bw[1]/2), bw)
            
            if(shape.model == "Gaussian")
            {
                xxx<-normix.bic(this.curve[,1], this.curve[,2], power=power, bw=bw, eliminate=component.eliminate, BIC.factor=BIC.factor)$param
                if(nrow(xxx) == 1)
                {
                    xxx<-c(xxx[1,1:2],xxx[1,2],xxx[1,3])
                }else{
                    xxx<-cbind(xxx[,1:2],xxx[,2],xxx[,3])
                }
            }else{
                xxx<-bigauss.mix(this.curve[,1], this.curve[,2], sigma.ratio.lim=sigma.ratio.lim, bw=bw, power=power, estim.method=estim.method, eliminate=component.eliminate, BIC.factor=BIC.factor)$param[,c(1,2,3,5)]
            }
            
            if(is.null(nrow(xxx)))
            {
                this.features<-rbind(this.features,c(median(this[,1]),xxx))
            }else{
                for(m in 1:nrow(xxx))
                {
                    this.d<-abs(this[,2]-xxx[m,1])
                    this.features<-rbind(this.features, c(mean(this[which(this.d==min(this.d)),1]),xxx[m,]))
                }
            }
        }
        
        #message(nnn)
    }
    this.features<-this.features[-1,]
    this.features<-this.features[order(this.features[,1], this.features[,2]),]
    this.features<-this.features[which(apply(this.features[,3:4],1,min)>sd.cut[1] & apply(this.features[,3:4],1,max)<sd.cut[2]),]
    rownames(this.features)<-NULL
    
    if(do.plot)
    {   
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mfrow=c(2,2))
        plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="",axes=FALSE)
        text(x=0,y=0,"Estimate peak \n area/location", cex=1.5)
        hist(mz.sd.rec,xlab="m/z SD", ylab="Frequency",main="m/z SD distribution")
        hist(c(this.features[,3],this.features[,4]), xlab="Retention time SD", ylab="Frequency", main="Retention time SD distribution")
        hist(log10(this.features[,5]), xlab="peak strength (log scale)", ylab="Frequency", main="Peak strength distribution")
    }
    
    return(this.features)
}
