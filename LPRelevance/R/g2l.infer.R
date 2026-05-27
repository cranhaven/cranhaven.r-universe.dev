g2l.infer <-
  function(X,z,X.test=NULL,m=c(6,8),alpha=.1,nsample=length(z), lp.reg.method='lm',
           fdr.curve.approx="direct",null.scale='QQ',
           ngrid=2000,centering=TRUE,fdr.method="locfdr", locfdr.df=10, coef.smooth='BIC',
           fdr.th.fixed=NULL,rel.null='custom',parallel=FALSE,...){

    extraparms<-list(...)
    if(is.null(extraparms$k) & lp.reg.method=='knn'){
      extraparms$k<-sqrt(length(z))
    }

    X<-as.matrix(X)
    n<-length(z)
    X.axe<-unique(X)
    if(is.null(X.test)){X.test<-X.axe}

    if(centering==FALSE){
      y<-z
      zmean<-rep(0,length(z))
    }else{
      Tx<-eLP.poly(X,m[1])
      centerproc<-z.lp.center(X,Tx,z,lp.reg.method,X.test,m,extraparms)
      y<-centerproc$y
      zmean<-centerproc$zmean
    }

    bre<-200;df<-locfdr.df
    if(rel.null=='th'){nulltype<-0}else{nulltype<-1}

    if(fdr.method=='locfdr'){
      if(null.scale=='locfdr'){
        w.full<-locfdr::locfdr(y,bre=bre,df=df,nulltype=nulltype,plot=0)
        sd0<-w.full$fp0[2*nulltype+1,2]
      }else if(null.scale=='IQR'){
        sd0<-IQR(y)/(2 * qnorm(0.75))
      }else if(null.scale=='QQ'){
        qn<-qqnorm(y,plot.it = FALSE)
        sd0 <- as.vector(MASS::rlm(qn$y~qn$x,psi =psi.bisquare,method='MM',maxit=100)$coef)[2]
      }
      w.full<-locfdr::locfdr(y/sd0,df=df,nulltype=0,plot=0)
      p0.pool<-c(0,sd0,w.full$fp0[1,3])
      fdr.pool<-approxfun(y,w.full$fdr,method='linear',rule=2)
      if(p0.pool[3]>1){p0.pool[3]<-1}
    }

    Ty<-eLP.poly(y,m[2])

    lpfdr.th<-list()
    lpfdr.fp0<-data.frame(mu=rep(0,nrow(X.test)),sd=rep(0,nrow(X.test)),pi0=rep(0,nrow(X.test)))

    bh.th<-rep(NA,nrow(X.test))

    lpfdr.th$ti<-lpfdr.th$u<-lpfdr.th$l<-rep(NA,nrow(X.test))
    lpfdr.grid<-lpfdr<-matrix(0,nrow(X.test),ngrid)

    simu_tags<-rep(0,length(z))
    z.fdr.tab<-data.frame(X=X,z=z,prob_null=0)

    if(parallel==TRUE){
      numCores<-round(detectCores()/2)
      cl<-makeCluster(numCores)
    }else{
      cl<-NULL
    }
    pb<-txtProgressBar(min = 0, max = nrow(X.test), initial = 0, style = 3)

    setTxtProgressBar(pb,0)

    for(i in 1:nrow(X.test)){
      tryCatch({
        #for multivar:
        iX.test<-X.test[i,]
        ind0<-which.min(apply(X.axe,1,function(x) sum(abs(x-iX.test))))
        x0<-matrix(X.axe[ind0,],nrow=length(ind0))
        zmean.ind<-which(apply(X,1,function(x) all(x==x0)))

        Lcoef<-LPcden(X,y,m,X.test=x0,method=lp.reg.method,lp.smooth=coef.smooth,k=extraparms$k)

        if(fdr.method=='locfdr'){
          if(sum(abs(Lcoef))==0){
            y.sample<-y
            ygrid<-seq(min(y.sample),max(y.sample),length.out=ngrid)
            lpfdr0<-fdr.pool
            parms0<-p0.pool
          }else{
            y.sample<-g2l.sampler(nsample,LP.par=t(Lcoef),Y=y,clusters=cl)
            ygrid<-seq(min(y.sample,y[zmean.ind]),max(y.sample,y[zmean.ind]),length.out=ngrid)

            if(null.scale=='locfdr'){
              w0 <- locfdr::locfdr(y.sample,bre=bre,df=df,nulltype=nulltype,plot=0)
              sd0 <-w0$fp0[2*nulltype+1,2]
            }else if(null.scale=='IQR'){
              sd0<-IQR(y.sample)/(2 * qnorm(0.75))
            }else if(null.scale=='QQ'){
              qn<-qqnorm(y.sample,plot.it = FALSE)
              sd0 <- as.vector(MASS::rlm(qn$y~qn$x,psi =psi.bisquare,method='MM',maxit=100)$coef)[2]
            }
            y.sample0<-y.sample/sd0
            w0 <- locfdr::locfdr(y.sample0,bre=bre,df=df,nulltype=0,plot=0)
            parms0<-c(0,sd0,w0$fp0[1,3])

            if(fdr.curve.approx=='direct'){
              lpfdr0<-approxfun(y.sample,w0$fdr,method='linear',rule=2)
            }else if(fdr.curve.approx=='indirect'){
              if(parms0[3]>1){parms0[3]<-1}
              fdr.y0<-fdr.pool(y)*(parms0[3]/p0.pool[3])*
                (dnorm(y,mean=parms0[1],sd=parms0[2])/
                   dnorm(y,mean=p0.pool[1],sd=p0.pool[2]))/
                as.numeric(1+Lcoef%*%t(Ty))
              fdr.y0[which(fdr.y0>=1)]<-1
              lpfdr0<-approxfun(y,fdr.y0,method='linear',rule=2)
            }
          }
          zgrid<-ygrid+zmean[zmean.ind[1]]
          x0fdr<-lpfdr0(ygrid)
          lpfdr[i,]<-x0fdr

          zfdr<-lpfdr0(y[zmean.ind])
          zfdr[zfdr>1]<-1
          zfdr[zfdr<0]<-0

          lpfdr.grid[i,]<-zgrid

          z.fdr.tab$prob_null[zmean.ind]<-zfdr
          lpfdr.fp0$mu[i]<-parms0[1]+zmean[zmean.ind[1]]
          lpfdr.fp0$sd[i]<-parms0[2]
          lpfdr.fp0$pi0[i]<-parms0[3]

          #threshold finding: (control alpha level for each x)
          if(is.null(fdr.th.fixed)){
            th_out<-fdr.thresh(y.sample,lpfdr0,alpha=alpha)
            th.fdr<-min(th_out$th.fdr,2*alpha) #threshold cannot be bigger than 2*alpha
          }else{
            th.fdr<-fdr.th.fixed
          }
          lpfdr.th$ti[i]<-th.fdr

          #finding upper limit:
          l0<--Inf;u0<-Inf
          zgrid.revind<-order(zgrid,decreasing =TRUE)
          for(ind in 1:length(zgrid)){
            if(is.na(th.fdr)){
              u0<-max(zgrid)
              break
            }else if(x0fdr[zgrid.revind[ind]]>th.fdr){
              if(ind>1){
                u0<-zgrid[zgrid.revind[ind-1]]
              }
              break
            }
          }
          #finding lower limit:
          for(ind in 1:length(zgrid)){
            if(is.na(th.fdr)){
              l0<-min(zgrid)
              break
            }else if(x0fdr[ind]>th.fdr){
              if(ind>1){
                l0<-zgrid[ind-1]
              }
              break
            }
          }
          if(!is.infinite(u0)){lpfdr.th$u[i]<-u0}
          if(!is.infinite(l0)){lpfdr.th$l[i]<-l0}

          simu_tags[zmean.ind]=(z[zmean.ind]<l0)+(z[zmean.ind]>u0)

        }else if(fdr.method=='BH'){
          if(sum(abs(Lcoef))==0){
            y.sample<-y
          }else{
            y.sample<-g2l.sampler(nsample,LP.par=t(Lcoef),Y=y,clusters=cl)
          }

          if(null.scale=='locfdr'){
            w0 <- locfdr::locfdr(y.sample,bre=bre,df=df,nulltype=nulltype,plot=0)
            parms0<-w0$fp0[2*nulltype+1,1:2]
          }else if(null.scale=='IQR'){
            sd0<-IQR(y.sample)/(2 * qnorm(0.75))
            parms0<-c(0,sd0)
          }else if(null.scale=='QQ'){
            qn<-qqnorm(y.sample,plot.it = FALSE)
            sd0 <- as.vector(MASS::rlm(qn$y~qn$x,psi =psi.bisquare,method='MM',maxit=100)$coef)[2]
            parms0<-c(0,sd0)
          }

          y.sample.mu<-parms0[1]
          y.sample.sig<-parms0[2]
          y.sample.std<-(y.sample-y.sample.mu)/y.sample.sig
          pvals<-2*pnorm(abs(y.sample.std),lower.tail=FALSE)
          bh.th[i]<-get_bh_threshold(pvals,alpha=alpha)

          y.s<-(y[zmean.ind]-y.sample.mu)/y.sample.sig
          p.obs<-2*pnorm(abs(y.s),lower.tail=FALSE)
          bh.tags.x0<-rep(0,length(p.obs))
          bh.tags.x0[p.obs<bh.th[i]]<-1
          simu_tags[zmean.ind]<-bh.tags.x0

        }
        setTxtProgressBar(pb,i)
      },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }


    if(parallel==TRUE){stopCluster(cl)}


    if(fdr.method=='locfdr'){
      out<-list(data=list(X=X,z=z,X.test=X.test,z.means=zmean,rej=simu_tags),
                fdr.z=z.fdr.tab,
                lp.m=m,
                lpfdr=lpfdr,
                z.grid=lpfdr.grid,
                lpfdr.fp0=lpfdr.fp0,
                lpfdr.th=lpfdr.th)
    }else if(fdr.method=='BH'){
      out<-list(data=list(X=X,z=z,X.test=X.test,z.means=zmean,rej=simu_tags),lp.m=m,
                BH.thresh=bh.th)
    }


    return(out)

  }