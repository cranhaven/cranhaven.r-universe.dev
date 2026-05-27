
rEB.proc <-
  function(X,z,X.target,z.target,m=c(4,6),nbag=NULL,centering=TRUE,
           lp.reg.method='lm',coef.smooth='BIC',
           nsample=min(length(z),2000), theta.set.prior=NULL,theta.set.post=NULL,
           LP.type='L2',g.method='DL',sd0=NULL,m.EB=8,parallel=FALSE,avg.method='mean',
           post.curve='HPD',post.alpha=.8,color='red',...){

    extraparms<-list(...)
    if(is.null(extraparms$k) & lp.reg.method=='knn'){
      extraparms$k<-sqrt(length(z))
    }

    X<-as.matrix(X)
    x0<-matrix(X.target,ncol=ncol(X))
    z0<-z.target
    if(is.null(theta.set.prior)){
      theta.set.prior<-seq(-2.5*sd(z),2.5*sd(z),length.out=100)
    }
    if(is.null(theta.set.post)){
      theta.set.post<-seq(z.target-2.5*sd(z),z.target+2.5*sd(z),length.out=100)
    }

    iter.flag=0
    if(!is.null(nbag)){if(nbag>=2){iter.flag=1}}
    LP.type<-LP.type[1]
    if(iter.flag==0){post.curve<-'HPD'}

    ##for ggplot variables, avoiding CRAN check notes:
    x<-y<-score<-lower<-upper<-ystart<-yend<-NULL

    out<-list()
    result<-list()
    result$post<-list(post.mean=NA,post.mean.sd=NA,HPD.interval=NA,post.alpha=post.alpha)

    if(iter.flag==0){
      nbag=1
      avg.method='mean'
    }
    reb_res<-LASER.rEB(X,z,x0,z0,gpar='sample',B=nbag,nsample=nsample,lp.reg.method=lp.reg.method,
                       post.alpha=post.alpha,centering=centering,
                       theta.set.prior=theta.set.prior, theta.set.post=theta.set.post,coef.smooth=coef.smooth,
                       sd0=sd0,m.obs=m,m.EB=m.EB,LP.type=LP.type,g.method=g.method,parallel=parallel,k=extraparms$k)
    if(avg.method=='median'){
      reb_res$prior$prior.fit$ds.prior<-reb_res$prior$prior.fit$prior.med
      reb_res$posterior$post.fit$ds.pos<-reb_res$posterior$post.fit$post.med
    }
    reb_res$prior$prior.fit$ds.prior[reb_res$prior$prior.fit$ds.prior<0]<-0
    reb_res$posterior$post.fit$ds.pos[reb_res$posterior$post.fit$ds.pos<0]<-0
    if(reb_res$stopflag==1){
      stop("process terminated due to error: tau estimate is zero, please use fixed model for parameter theta.")
    }

    result$post$post.mode<-reb_res$posterior$post.fit$theta.vals[which.max(reb_res$posterior$post.fit$ds.pos)]
    result$post$post.mean.sd<-reb_res$posterior$DS.mean.sd
    result$post$post.mean=reb_res$post$DS.mean
    result$sd0=reb_res$prior$sd_est
    result$prior<-list(g.par=reb_res$prior$g.par,
                       LP.coef=reb_res$prior$LP.par,g.method=reb_res$prior$g.method
    )
    #extra
    #result$post$meanlist=reb_res$post$meanlist
    #result$post$fits<-reb_res$post$fitlist
    #result$prior$fits<-reb_res$prior$fitlist

    out$result<-result


    ##########plotting###########
    if(avg.method=='median'){
      lreg.fit<-smooth.spline(x=reb_res$prior$prior.fit$theta.vals,y=reb_res$prior$prior.fit$ds.prior,df=10)
      smoothprior<-fitted(lreg.fit)
      smoothprior[smoothprior<0]=0
      lreg.fit<-smooth.spline(x=reb_res$posterior$post.fit$theta.vals,y=reb_res$posterior$post.fit$ds.pos,df=10)
      smoothpost<-fitted(lreg.fit)
      smoothpost[smoothpost<0]=0
      d0_prior=data.frame(x=reb_res$prior$prior.fit$theta.vals,y=smoothprior)
      d0_post=data.frame(x=reb_res$posterior$post.fit$theta.vals,y=smoothpost)
    }else if(avg.method=='mean'){
      d0_prior=data.frame(x=reb_res$prior$prior.fit$theta.vals,y=reb_res$prior$prior.fit$ds.prior)
      d0_post=data.frame(x=reb_res$posterior$post.fit$theta.vals,y=reb_res$posterior$post.fit$ds.pos)
    }


    p_prior<-ggplot2::ggplot(data=d0_prior,aes(x=x,y=y))+geom_line(size=.8,color=color)+
      ylab('Estimated Prior')+xlab(expression(theta))+ggtitle('')+
      theme(text=element_text(size=13),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=14),
            legend.position="none",
            legend.title=element_blank())

    p_post<-ggplot2::ggplot()+geom_line(data=d0_post,aes(x=x,y=y),size=.8,color=color)+
      ylab('Posterior Distribution')+xlab(expression(theta))+ggtitle('')+
      theme(text=element_text(size=13),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=14),
            legend.position="none",
            legend.title=element_blank())

    if(post.curve=='HPD'){
      OBJ<-reb_res$posterior
      samp.post <- sample(OBJ$post.fit$ds.pos, 1e5, replace = TRUE,prob = OBJ$post.fit$ds.pos)
      crit.post <- quantile(samp.post, 1-post.alpha)
      post.interval <- c(min(OBJ$post.fit$theta.vals[OBJ$post.fit$ds.pos >=crit.post]),
                         max(OBJ$post.fit$theta.vals[OBJ$post.fit$ds.pos >=crit.post]))
      ci.start.ind<-which.min(abs(post.interval[1]-OBJ$post.fit$theta.vals))
      ci.end.ind<-which.min(abs(post.interval[2]-OBJ$post.fit$theta.vals))

      d_area<-d0_post[ci.start.ind:ci.end.ind,]
      p_post<-p_post+geom_area(data=d_area,aes(x=x,y=y),fill=color,alpha=.4)
      out$result$post$HPD.interval<-post.interval

    }else if(post.curve=='band'){

      d0_band<-data.frame(x=d0_post$x,
                          upper=reb_res$posterior$post.fit$post.cbandu,
                          lower=reb_res$posterior$post.fit$post.cbandl)

      p_post<-p_post+geom_ribbon(data=d0_band,aes(x=x,ymin=lower,ymax=upper),fill=color,alpha=.3)+
        geom_hline(yintercept=0)

    }

    p_post<-p_post+geom_point(data=data.frame(x=result$post$post.mode,y=0),
                              aes(x=x,y=y),color=color,size=4,shape=18)
    #if(iter.flag!=0){
    #  d_seg<-data.frame(x=c(reb_res$posterior$DS.mean-reb_res$posterior$DS.mean.sd,
    #                        reb_res$posterior$DS.mean+reb_res$posterior$DS.mean.sd),
    #                    ystart=-.025,yend=.025)
    #  p_post<-p_post+geom_segment(data=d_seg,aes(x=x,y=ystart,xend=x,yend=yend),color=color)
    #}

    out$plots<-list(rEB.prior=p_prior,rEB.post=p_post)

    return(out)
  }