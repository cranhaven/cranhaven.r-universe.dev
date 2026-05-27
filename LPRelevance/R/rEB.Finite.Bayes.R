rEB.Finite.Bayes<-function(X,z,X.target,z.target,m=c(4,6),m.EB=8, B=10, centering=TRUE,
                      nsample=min(1000,length(z)), g.method='DL',LP.type='L2',  sd0=NULL,
                      theta.set.prior=seq(-2.5*sd(z),2.5*sd(z),length.out=500),
                      theta.set.post=seq(z.target-2.5*sd(z),z.target+2.5*sd(z),length.out=500),
                      post.alpha=0.8,  plot=TRUE, ...){
  max.iter=2000
  extraparms=list(...)
  extraparms$X=X;extraparms$z=z;extraparms$X.target=X.target;extraparms$m=m;extraparms$nsample=nsample;extraparms$centering=centering
  #1. zsample and sd0 from LASER
  pb<-txtProgressBar(min=0,max=B,style=3)
  setTxtProgressBar(pb,0)
  samplegen<-do.call(LASER,args=extraparms)
  z.sample<-samplegen$data
  if(is.null(sd0)){
    sd0<-IQR(z.sample)/1.35   #sdfun(z.sample)
  }
  data.z <- cbind(z.sample,rep(sd0,length(z.sample)))
  #2. g parameters and DS.prior
  reb.start <- BayesGOF::gMLE.nn(data.z[,1], data.z[,2],  g.method)$estimate
  reb.ds.L2 <- BayesGOF::DS.prior(data.z, max.m = m.EB, g.par = reb.start, family = "Normal", LP.type = LP.type)
  #3. based on DS.prior
  prior.list<-post.list<-list()
  viter=0
  setTxtProgressBar(pb,0.1)
  for(iter in 1:max.iter){
    #theta_i generated using DS.sampler, size: nsample
    te_sample<-DS.sampler(nsample, reb.start, reb.ds.L2$LP.par, 'Normal', LP.type)
    #4.y_i from theta_i
    y_sample=sapply(te_sample,function(x){stats::rnorm(1,mean=x,sd=sd0)})
    #5.perform bayes analysis on y_i
    #sd0_y<-IQR(y_sample)/1.35
    data.y <- cbind(y_sample,rep(sd0,length(y_sample)))
    reb.start0 <- BayesGOF::gMLE.nn(data.y[,1], data.y[,2],  g.method)$estimate

    if(reb.start0[2]!=0){
      viter=viter+1
      reb.ds.iter <- BayesGOF::DS.prior(data.y, max.m = m.EB, g.par = reb.start0, family = "Normal", LP.type = LP.type)
      priorfit_parm=approx(reb.ds.iter$prior.fit$theta.vals,reb.ds.iter$prior.fit$parm.prior,
                           xout=theta.set.prior,method='linear',rule=2)$y
      prior.list[[viter]]<-reb.ds.iter
      prior.list[[viter]]$prior.fit=data.frame(theta.vals=theta.set.prior,parm.prior=priorfit_parm)
      if(is.null(reb.ds.iter$prior.fit$ds.prior)){
        prior.list[[viter]]$prior.fit$ds.prior=prior.list[[viter]]$prior.fit$parm.prior
      }else{
        priorfit_ds=approx(reb.ds.iter$prior.fit$theta.vals,reb.ds.iter$prior.fit$ds.prior,
                           xout=theta.set.prior,method='linear',rule=2)$y
        prior.list[[viter]]$prior.fit$ds.prior=priorfit_ds
      }
      reb.micro.iter_1 <- BayesGOF::DS.micro.inf(reb.ds.iter, y.0=z.target, n.0=sd0)
      reb.micro.iter_fit<-LP.post.conv(theta.set.post, reb.ds.iter, y.0=z.target, n.0=sd0)
      post.list[[viter]]<-reb.micro.iter_1
      post.list[[viter]]$post.fit<-reb.micro.iter_fit
      if(is.null(post.list[[viter]]$post.fit$ds.pos)){
        post.list[[viter]]$post.fit$ds.pos=post.list[[viter]]$post.fit$parm.pos
      }
      setTxtProgressBar(pb,viter)
      if(viter>=B){
        break
      }
      # repeat 3~5 for B times
    }

  }
  # average the curves and parameters
  prior.curve<-data.frame(
    theta.vals=theta.set.prior,
    parm.prior=apply(sapply(prior.list,function(x){x$prior.fit$parm.prior}),1,mean),
    ds.prior=apply(sapply(prior.list,function(x){x$prior.fit$ds.prior}),1,mean)
  )
  post.curve<-data.frame(
    theta.vals=theta.set.post,
    parm.post=apply(sapply(post.list,function(x){x$post.fit$parm.pos}),1,mean),
    ds.pos=apply(sapply(post.list,function(x){x$post.fit$ds.pos}),1,mean)
  )
  post.curve$ds.pos[post.curve$ds.pos<0]<-0
  prior.curve$ds.prior[prior.curve$ds.prior<0]<-0
  post.mean=mean(sapply(post.list,function(x){x$DS.mean}))
  post.mean.sd=sd(sapply(post.list,function(x){x$DS.mean}))

  #HPD
  samp.post <- sample(post.curve$ds.pos, 1e5, replace = TRUE,prob = post.curve$ds.pos)
  crit.post <- quantile(samp.post, 1-post.alpha)
  hpd.interval<- c(min(post.curve$theta.vals[post.curve$ds.pos >=crit.post]),
                   max(post.curve$theta.vals[post.curve$ds.pos >=crit.post]))


  prior=list(prior.fit=prior.curve)
  posterior=list(post.fit=post.curve,
                 post.mode=post.curve$theta.vals[which.max(post.curve$ds.pos)],
                 post.mean=post.mean, post.mean.sd=post.mean.sd,
                 HPD.interval=hpd.interval
  )

  out=list(prior=prior,posterior=posterior,g.par=reb.start,sd0=sd0,sample=z.sample,LP.coef=samplegen$LPcoef)

  #========================plotting=====================#
  ##for ggplot variables, avoiding CRAN check notes:
  x<-y<-score<-lower<-upper<-ystart<-yend<-NULL
  if(plot==TRUE){
    d0_prior=data.frame(x=prior.curve$theta.vals,y=prior.curve$ds.prior)
    d0_post=data.frame(x=post.curve$theta.vals,y=post.curve$ds.pos)

    p_prior<-ggplot2::ggplot(data=d0_prior,aes(x=x,y=y))+geom_line(size=.8,color='red')+
      ylab('Estimated Prior')+xlab(expression(theta))+ggtitle('')+
      theme(text=element_text(size=13),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=14),
            legend.position="none",
            legend.title=element_blank())

    p_post<-ggplot2::ggplot()+geom_line(data=d0_post,aes(x=x,y=y),size=.8,color='red')+
      ylab('Posterior Distribution')+xlab(expression(theta))+ggtitle('')+
      theme(text=element_text(size=13),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=14),
            legend.position="none",
            legend.title=element_blank())

    ci.start.ind<-which.min(abs(hpd.interval[1]-post.curve$theta.vals))
    ci.end.ind<-which.min(abs(hpd.interval[2]-post.curve$theta.vals))
    d_area<-d0_post[ci.start.ind:ci.end.ind,]
    p_post<-p_post+geom_area(data=d_area,aes(x=x,y=y),fill='red',alpha=.4)
    p_post<-p_post+geom_point(data=data.frame(x=posterior$post.mode,y=0),
                              aes(x=x,y=y),color='red',size=4,shape=18)


    out$plots=list(prior=p_prior,post=p_post)
  }


  return(out)
}
