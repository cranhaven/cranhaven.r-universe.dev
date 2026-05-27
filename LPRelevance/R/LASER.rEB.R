
LASER.rEB <-
  function(X,z,X0,z0,gpar='sample', B=50, nsample=length(z), lp.reg.method='lm',
           post.alpha=.8,centering=TRUE,coef.smooth='BIC',
           theta.set.prior=seq(-2.5*sd(z),2.5*sd(z),length.out=100),LP.type ='L2',g.method='DL',
           theta.set.post=seq(z0-2.5*sd(z),z0+2.5*sd(z),length.out=100),sd0=NULL,m.obs=c(4,6),m.EB=6,parallel=FALSE,
           max.iter=B+1000,...){

    #use median to aggregate the posterior means and use IQR/1.3489 for sd

    extraparms<-list(...)
    if(is.null(extraparms$k) & lp.reg.method=='knn'){
      extraparms$k<-sqrt(length(z))
    }

    X<-as.matrix(X)
    n<-length(z)
    z.target<-z0
    X.target<-matrix(X0,ncol=ncol(X))

    zm.target<-0
    zmean<-rep(0,length(z))
    if(centering==FALSE){
      y<-z
    }else{
      Tx<-eLP.poly(X,m.obs[1])
      centerproc<-z.lp.center(X,Tx,z,lp.reg.method,X.target,m.obs,extraparms)
      y<-centerproc$y
      z.mu.test<-centerproc$z.mu.test
      zmean<-centerproc$zmean
      zm.target<-as.numeric(z.mu.test)
    }


    Lcoef<-LPcden(X,y,m=m.obs,X.test=X0,method=lp.reg.method,lp.smooth=coef.smooth,k=extraparms$k)
    y.target<-z.target-zm.target

    no_iter_flag<-0;global_flag<-0 ##when using global observation, break the loop
    if(sum(abs(Lcoef))==0){no_iter_flag<-1;global_flag<-1;B=1}

    if(parallel==TRUE){
      numCores<-round(detectCores()/2)
      cl<-makeCluster(numCores)
    }else{cl<-NULL}

    prior.fit.list<-matrix(NA,B,length(theta.set.prior))
    prior.parm.list<-matrix(NA,B,length(theta.set.prior))
    post.fit.list<-matrix(NA,B,length(theta.set.post))
    post.parm.list<-matrix(NA,B,length(theta.set.prior))
    post.mean<-rep(NA,B)
    sd.list<-rep(NA,B)
    reb.ds.L2<-NA
    reb.micro.z0_1<-NA
    nval<-0

    pb<-txtProgressBar(min=0,max=B,style=3)
    stop_flag=0
    for(iter in 1:max.iter){
      tryCatch({
        if(nval>=B){
          break;
        }
        check.passed<-0
        if(global_flag==1){
          z.sample<-y+zm.target
        }else{
          y.sample<-g2l.sampler(nsample,LP.par=t(Lcoef),Y=y,clusters=cl)
          z.sample<-y.sample+zm.target
        }
        if(is.null(sd0)){
          #if(length(z.sample)>=500){
          #  sd0<-locfdr::locfdr(z.sample,bre=200,df=10,nulltype=1,plot=0)$fp0[3,2]
          #}else{
          sd0<-IQR(z.sample)/1.3489
          #}
        }

        if(gpar=='sample'){
          data.z <- cbind(z.sample,rep(sd0,nsample))
        }else{
          #if(length(z)>=500){
          #  sd0<-locfdr::locfdr(z,bre=200,df=10,nulltzpe=1,plot=0)$fp0[3,2]
          #}else{
          sd0<-IQR(z)/1.3489
          #}
          data.z <- cbind(z,rep(sd0,length(z)))
        }

        reb.start <- BayesGOF::gMLE.nn(data.z[,1], data.z[,2], method = g.method)$estimate
        if(reb.start[2]==0 & no_iter_flag==1){
          gnn.method.found=0
          gmethod.list=c('DL','SJ','REML','MoM')
          for(gnn_id in 1:length(gmethod.list)){
            reb.start <- BayesGOF::gMLE.nn(data.z[,1], data.z[,2], method = gmethod.list[gnn_id])$estimate
            if(reb.start[2]!=0){
              gnn.method.found=1
              g.method=gmethod.list[gnn_id]
              break
            }
          }
          if(gnn.method.found!=1){
            stop_flag=1
            stop("tau estimate is zero, please use fixed model for parameter theta.")
          }
        }

        if(reb.start[2]!=0){
          reb.ds.L2 <- BayesGOF::DS.prior(data.z, max.m = m.EB, g.par = reb.start, family = "Normal", LP.type = LP.type)
          if(sum(abs(reb.ds.L2$LP.par))<50){
            check.passed<-1
          }else if(no_iter_flag==1){
            reb.ds.L2 <- BayesGOF::DS.prior(data.z, max.m = m.EB, g.par = reb.start, family = "Normal", LP.type = 'L2')
            print("using LP.type='L2'")
          }
        }

        if(check.passed==1){

          nval<-nval+1
          sd.list[nval]=sd0
          reb.micro.z0_1 <- BayesGOF::DS.micro.inf(reb.ds.L2, y.0=z.target, n.0=sd0)
          reb.micro.z0<-LP.post.conv(theta.set.post, reb.ds.L2, y.0=z.target, n.0=sd0)
          if(is.null(reb.ds.L2$prior.fit$ds.prior)){
            reb.ds.L2$prior.fit$ds.prior<-reb.ds.L2$prior.fit$parm.prior
          }
          if(is.null(reb.micro.z0$ds.pos)){
            reb.micro.z0$ds.pos<-reb.micro.z0$parm.pos
          }

          prior.fit.list[nval,]<-approx(x=reb.ds.L2$prior.fit$theta.vals,y=reb.ds.L2$prior.fit$ds.prior,
                                        xout=theta.set.prior,method='linear',rule=2)$y
          prior.parm.list[nval,]<-approx(x=reb.ds.L2$prior.fit$theta.vals,y=reb.ds.L2$prior.fit$parm.prior,
                                         xout=theta.set.prior,method='linear',rule=2)$y
          post.fit.list[nval,]<-reb.micro.z0$ds.pos
          post.parm.list[nval,]<-reb.micro.z0$parm.pos
          post.mean[ nval]<-reb.micro.z0_1$DS.mean
          setTxtProgressBar(pb,nval)
        }
        if(no_iter_flag==1){break}
      },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }

    if(parallel==TRUE){
      stopCluster(cl)
    }

    if(nval>=2){
      ds.prior.avg<-colSums(prior.fit.list[1:nval,])/nval
      ds.prior.med<-apply(prior.fit.list[1:nval,],2,FUN=median)
      ds.prior.sd<-apply(prior.fit.list[1:nval,],2,FUN=sd)
      parm.prior.avg<-colSums(prior.parm.list[1:nval,])/nval
      ds.post.avg<-colSums(post.fit.list[1:nval,])/nval
      ds.post.med<-apply(post.fit.list[1:nval,],2,FUN=median)
      ds.post.sd<-apply(post.fit.list[1:nval,],2,FUN=sd)
      ds.post.cbandl<-apply(post.fit.list[1:nval,],2,FUN=function(x){as.numeric(quantile(x, (1-post.alpha)/2))})
      ds.post.cbandu<-apply(post.fit.list[1:nval,],2,FUN=function(x){as.numeric(quantile(x, 1-(1-post.alpha)/2))})
      parm.post.avg<-colSums(post.parm.list[1:nval,])/nval
      ds.mean.avg<-median(post.mean[1:nval])
      ds.mean.sd<-IQR(post.mean[1:nval])/1.3489
      out.sd0<-median(sd.list)
    }else{
      ds.prior.avg<-prior.fit.list[1,]
      ds.prior.med<-prior.fit.list[1,]
      ds.prior.sd<-NA
      ds.post.cbandl<-ds.prior.avg
      ds.post.cbandu<-ds.prior.avg
      parm.prior.avg<-prior.parm.list[1,]
      ds.post.avg<-post.fit.list[1,]
      ds.post.med<-post.fit.list[1,]
      ds.post.sd<-NA
      parm.post.avg<-post.parm.list[1,]
      ds.mean.avg<-post.mean[1]
      ds.mean.sd<-NA
      out.sd0<-sd0
    }


    prior<-reb.ds.L2
    post<-reb.micro.z0_1

    prior.fit=data.frame(theta.vals=theta.set.prior,ds.prior=ds.prior.avg,parm.prior=parm.prior.avg,
                         prior.med=ds.prior.med,prior.sd=ds.prior.sd)
    post.fit=data.frame(theta.vals=theta.set.post,ds.pos=ds.post.avg,parm.pos=parm.post.avg,
                        post.med=ds.post.med,post.sd=ds.post.sd,post.cbandl=ds.post.cbandl,post.cbandu=ds.post.cbandu)

    prior$prior.fit<-prior.fit
    prior$g.method<-g.method
    prior$sd_est<-sd0
    post$post.fit<-post.fit
    post$DS.mean<-ds.mean.avg
    post$DS.mean.sd<-ds.mean.sd
    post$DS.mode<-theta.set.post[which.max(post.fit$ds.pos)]
    #extra
    #post$meanlist=post.mean
    #prior$fitlist<-prior.fit.list
    #post$fitlist<-post.fit.list

    out<-list(prior=prior,posterior=post,nvalid=nval,stopflag=stop_flag)
    return(out)
  }
