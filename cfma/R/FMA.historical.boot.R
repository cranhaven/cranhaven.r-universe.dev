FMA.historical.boot <-
function(Z,M,Y,delta.grid1=1,delta.grid2=1,delta.grid3=1,intercept=TRUE,basis1=NULL,Ld2.basis1=NULL,basis2=NULL,Ld2.basis2=NULL,basis.type=c("fourier"),
                               nbasis1=3,nbasis2=3,timeinv=c(0,1),timegrids=NULL,lambda1.m=0.01,lambda2.m=0.01,lambda1.y=0.01,lambda2.y=0.01,
                               sims=1000,boot=TRUE,boot.ci.type=c("bca","perc"),conf.level=0.95,verbose=TRUE)
{
  # delta.grid1: M~Z time interval
  # delta.grid2: Y~Z time interval
  # delta.grid3: Y~M time interval
  
  N<-dim(Z)[1]             # # of subject
  ntp<-dim(Z)[2]           # # of time points
  
  if(is.null(timegrids))
  {
    timegrids<-seq(timeinv[1],timeinv[2],length.out=ntp)
  }
  
  # basis functions
  if(is.null(basis1))
  {
    if(basis.type[1]=="fourier")
    {
      basis1<-fourier.basis(timeinv=timeinv,ntp=ntp,nbasis=nbasis1)
      
      Ld2.basis1<-Ld2.fourier(timeinv=timeinv,ntp=ntp,nbasis=nbasis1)
    }
  }else
  {
    nbasis1<-ncol(basis1)
  }
  if(is.null(basis2))
  {
    if(basis.type[1]=="fourier")
    {
      basis2<-fourier.basis(timeinv=timeinv,ntp=ntp,nbasis=nbasis2)
      
      Ld2.basis2<-Ld2.fourier(timeinv=timeinv,ntp=ntp,nbasis=nbasis2)
    }
  }else
  {
    nbasis2<-ncol(basis2)
  }
  
  # M model
  fit.m<-FDA.historical(Z,M,delta.grid=delta.grid1,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                        nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda1.m,lambda2=lambda2.m)
  # Y model
  fit.y<-FDA.historical2(X1=Z,X2=M,Y,delta.grid1=delta.grid2,delta.grid2=delta.grid3,intercept=intercept,
                         basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                         nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda1.y,lambda2=lambda2.y)
  
  if(boot)
  {
    coef.alpha=coef.gamma=coef.beta<-array(NA,c(ncol(fit.m$basis1),ncol(fit.m$basis2),sims))
    c.alpha=c.gamma=c.beta<-array(NA,c(ntp,ntp,sims))
    c.IE=c.DE<-matrix(NA,sims,ntp)
    for(b in 1:sims)
    {
      idx.tmp<-sample(1:N,N,replace=TRUE)
      
      Ztmp<-Z[idx.tmp,]
      Mtmp<-M[idx.tmp,]
      Ytmp<-Y[idx.tmp,]
      
      re.tmp<-FMA.historical(Ztmp,Mtmp,Ytmp,delta.grid1=delta.grid1,delta.grid2=delta.grid2,delta.grid3=delta.grid3,intercept=intercept,
                             basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,nbasis1=nbasis1,nbasis2=nbasis2,
                             timeinv=timeinv,timegrids=timegrids,lambda1.m=lambda1.m,lambda2.m=lambda2.m,lambda1.y=lambda1.y,lambda2.y=lambda2.y)
      
      coef.alpha[,,b]<-re.tmp$M$coefficients$alpha
      coef.gamma[,,b]<-re.tmp$Y$coefficients$gamma
      coef.beta[,,b]<-re.tmp$Y$coefficients$beta
      
      c.alpha[,,b]<-re.tmp$M$curve$alpha
      c.gamma[,,b]<-re.tmp$Y$curve$gamma
      c.beta[,,b]<-re.tmp$Y$curve$beta
      
      c.IE[b,]<-re.tmp$IE$curve
      c.DE[b,]<-re.tmp$DE$curve
      
      if(verbose)
      {
        print(paste0("Bootstrap sample ",b))
      }
    }
    se.alpha<-apply(coef.alpha,c(1,2),sd,na.rm=TRUE)
    se.gamma<-apply(coef.gamma,c(1,2),sd,na.rm=TRUE)
    se.beta<-apply(coef.beta,c(1,2),sd,na.rm=TRUE)
    
    se.c.alpha<-apply(c.alpha,c(1,2),sd,na.rm=TRUE)
    se.c.gamma<-apply(c.gamma,c(1,2),sd,na.rm=TRUE)
    se.c.beta<-apply(c.beta,c(1,2),sd,na.rm=TRUE)
    se.c.IE<-apply(c.IE,2,sd,na.rm=TRUE)
    se.c.DE<-apply(c.DE,2,sd,na.rm=TRUE)
    
    if(boot.ci.type[1]=="bca")
    {
      ci.alpha<-apply(coef.alpha,c(1,2),BC.CI,sims=sims,conf.level=conf.level)
      ci.gamma<-apply(coef.gamma,c(1,2),BC.CI,sims=sims,conf.level=conf.level)
      ci.beta<-apply(coef.beta,c(1,2),BC.CI,sims=sims,conf.level=conf.level)
      
      ci.c.alpha<-apply(c.alpha,c(1,2),BC.CI,sims=sims,conf.level=conf.level)
      ci.c.gamma<-apply(c.gamma,c(1,2),BC.CI,sims=sims,conf.level=conf.level)
      ci.c.beta<-apply(c.beta,c(1,2),BC.CI,sims=sims,conf.level=conf.level)
      ci.c.IE<-apply(c.IE,2,BC.CI,sims=sims,conf.level=conf.level)
      ci.c.DE<-apply(c.DE,2,BC.CI,sims=sims,conf.level=conf.level)
    }
    if(boot.ci.type[1]=="perc")
    {
      ci.alpha<-apply(coef.alpha,c(1,2),quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.gamma<-apply(coef.gamma,c(1,2),quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.beta<-apply(coef.beta,c(1,2),quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      
      ci.c.alpha<-apply(c.alpha,c(1,2),quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.c.gamma<-apply(c.gamma,c(1,2),quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.c.beta<-apply(c.beta,c(1,2),quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.c.IE<-apply(c.IE,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.c.DE<-apply(c.DE,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
    }
    
    re.alpha<-list(Estimate=apply(coef.alpha,c(1,2),mean,na.rm=TRUE),SE=se.alpha,LB=ci.alpha[1,,],UB=ci.alpha[2,,])
    re.gamma<-list(Estimate=apply(coef.gamma,c(1,2),mean,na.rm=TRUE),SE=se.gamma,LB=ci.gamma[1,,],UB=ci.gamma[2,,])
    re.beta<-list(Estimate=apply(coef.beta,c(1,2),mean,na.rm=TRUE),SE=se.beta,LB=ci.beta[1,,],UB=ci.beta[2,,])
    
    curve.alpha<-list(Estimate=apply(c.alpha,c(1,2),mean,na.rm=TRUE),SE=se.c.alpha,LB=ci.c.alpha[1,,],UB=ci.c.alpha[2,,])
    curve.gamma<-list(Estimate=apply(c.gamma,c(1,2),mean,na.rm=TRUE),SE=se.c.gamma,LB=ci.c.gamma[1,,],UB=ci.c.gamma[2,,])
    curve.beta<-list(Estimate=apply(c.beta,c(1,2),mean,na.rm=TRUE),SE=se.c.beta,LB=ci.c.beta[1,,],UB=ci.c.beta[2,,])
    
    curve.IE<-rbind(apply(c.IE,2,mean,na.rm=TRUE),se.c.IE,ci.c.IE)
    curve.DE<-rbind(apply(c.DE,2,mean,na.rm=TRUE),se.c.DE,ci.c.DE)
    rownames(curve.IE)=rownames(curve.DE)<-c("Estimate","SE","LB","UB")
    
    re<-list(alpha=list(coefficients=re.alpha,curve=curve.alpha),gamma=list(coefficients=re.gamma,curve=curve.gamma),
             beta=list(coefficients=re.beta,curve=curve.beta),IE=list(curve=curve.IE),DE=list(curve=curve.DE))
    
    return(re)
  }else
  {
    return(FMA.historical(Z,M,Y,delta.grid1=delta.grid1,delta.grid2=delta.grid2,delta.grid3=delta.grid3,intercept=intercept,
                          basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,nbasis1=nbasis1,nbasis2=nbasis2,
                          timeinv=timeinv,timegrids=timegrids,lambda1.m=lambda1.m,lambda2.m=lambda2.m,lambda1.y=lambda1.y,lambda2.y=lambda2.y))
  }
}
