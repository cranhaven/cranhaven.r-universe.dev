FMA.concurrent.CV.boot <-
function(Z,M,Y,intercept=TRUE,basis=NULL,Ld2.basis=NULL,basis.type=c("fourier"),nbasis=3,timeinv=c(0,1),timegrids=NULL,
                                 lambda=10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1]),nfolds=5,
                                 sims=1000,boot=TRUE,boot.ci.type=c("bca","perc"),conf.level=0.95,verbose=TRUE)
{
  N<-nrow(Z)             # # of subject
  ntp<-ncol(Z)           # # of time points
  
  if(is.null(timegrids))
  {
    timegrids<-seq(timeinv[1],timeinv[2],length.out=ntp)
  }
  
  # M model
  fit.m<-FDA.concurrent.CV(Z,M,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,lambda=lambda,nfolds=nfolds,verbose=FALSE)
  lambda.m<-fit.m$lambda
  # Y model
  Xtmp<-array(NA,c(N,ntp,2))
  Xtmp[,,1]<-Z
  Xtmp[,,2]<-M
  fit.y<-FDA.concurrent.CV(Xtmp,Y,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,lambda=lambda,nfolds=nfolds,verbose=FALSE)
  lambda.y<-fit.y$lambda
  
  if(boot)
  {
    coef.alpha=coef.beta=coef.gamma=coef.IE<-matrix(NA,sims,ncol(fit.m$basis))
    c.alpha=c.beta=c.gamma=c.IE<-matrix(NA,sims,ntp)
    for(b in 1:sims)
    {
      idx.tmp<-sample(1:N,N,replace=TRUE)
      
      Ztmp<-Z[idx.tmp,]
      Mtmp<-M[idx.tmp,]
      Ytmp<-Y[idx.tmp,]
      
      re.tmp<-FMA.concurrent(Ztmp,Mtmp,Ytmp,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,
                             lambda.m=lambda.m,lambda.y=lambda.y)
      
      coef.alpha[b,]<-re.tmp$M$coefficients["Z",]
      coef.gamma[b,]<-re.tmp$Y$coefficients["Z",]
      coef.beta[b,]<-re.tmp$Y$coefficients["M",]
      
      coef.IE[b,]<-re.tmp$IE$coefficients
      
      c.alpha[b,]<-re.tmp$M$curve["Z",]
      c.gamma[b,]<-re.tmp$Y$curve["Z",]
      c.beta[b,]<-re.tmp$Y$curve["M",]
      c.IE[b,]<-re.tmp$IE$curve
      
      if(verbose)
      {
        print(paste0("Bootstrap sample ",b))
      }
    }
    se.alpha<-apply(coef.alpha,2,sd,na.rm=TRUE)
    se.gamma<-apply(coef.gamma,2,sd,na.rm=TRUE)
    se.beta<-apply(coef.beta,2,sd,na.rm=TRUE)
    se.IE<-apply(coef.IE,2,sd,na.rm=TRUE)
    
    se.c.alpha<-apply(c.alpha,2,sd,na.rm=TRUE)
    se.c.gamma<-apply(c.gamma,2,sd,na.rm=TRUE)
    se.c.beta<-apply(c.beta,2,sd,na.rm=TRUE)
    se.c.IE<-apply(c.IE,2,sd,na.rm=TRUE)
    
    if(boot.ci.type[1]=="bca")
    {
      ci.alpha<-apply(coef.alpha,2,BC.CI,sims=sims,conf.level=conf.level)
      ci.gamma<-apply(coef.gamma,2,BC.CI,sims=sims,conf.level=conf.level)
      ci.beta<-apply(coef.beta,2,BC.CI,sims=sims,conf.level=conf.level)
      ci.IE<-apply(coef.IE,2,BC.CI,sims=sims,conf.level=conf.level)
      
      ci.c.alpha<-apply(c.alpha,2,BC.CI,sims=sims,conf.level=conf.level)
      ci.c.gamma<-apply(c.gamma,2,BC.CI,sims=sims,conf.level=conf.level)
      ci.c.beta<-apply(c.beta,2,BC.CI,sims=sims,conf.level=conf.level)
      ci.c.IE<-apply(c.IE,2,BC.CI,sims=sims,conf.level=conf.level)
    }
    if(boot.ci.type[1]=="perc")
    {
      ci.alpha<-apply(coef.alpha,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.gamma<-apply(coef.gamma,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.beta<-apply(coef.beta,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.IE<-apply(coef.IE,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      
      ci.c.alpha<-apply(c.alpha,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.c.gamma<-apply(c.gamma,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.c.beta<-apply(c.beta,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
      ci.c.IE<-apply(c.IE,2,quantile,probs=c((1-conf.level)/2,1-(1-conf.level)/2),na.rm=TRUE)
    }
    
    re.alpha<-rbind(apply(coef.alpha,2,mean,na.rm=TRUE),se.alpha,ci.alpha)
    re.gamma<-rbind(apply(coef.gamma,2,mean,na.rm=TRUE),se.gamma,ci.gamma)
    re.beta<-rbind(apply(coef.beta,2,mean,na.rm=TRUE),se.beta,ci.beta)
    re.IE<-rbind(apply(coef.IE,2,mean,na.rm=TRUE),se.IE,ci.IE)
    rownames(re.alpha)=rownames(re.gamma)=rownames(re.beta)=rownames(re.IE)<-c("Estimate","SE","LB","UB")
    colnames(re.alpha)=colnames(re.gamma)=colnames(re.beta)=colnames(re.IE)<-paste0("basis",1:ncol(fit.m$basis))
    
    curve.alpha<-rbind(apply(c.alpha,2,mean,na.rm=TRUE),se.c.alpha,ci.c.alpha)
    curve.gamma<-rbind(apply(c.gamma,2,mean,na.rm=TRUE),se.c.gamma,ci.c.gamma)
    curve.beta<-rbind(apply(c.beta,2,mean,na.rm=TRUE),se.c.beta,ci.c.beta)
    curve.IE<-rbind(apply(c.IE,2,mean,na.rm=TRUE),se.c.IE,ci.c.IE)
    rownames(curve.alpha)=rownames(curve.gamma)=rownames(curve.beta)=rownames(curve.IE)<-c("Estimate","SE","LB","UB")
    
    re<-list(alpha=list(coefficients=re.alpha,curve=curve.alpha),gamma=list(coefficients=re.gamma,curve=curve.gamma),beta=list(coefficients=re.beta,curve=curve.beta),
             IE=list(coefficients=re.IE,curve=curve.IE),DE=list(coefficients=re.gamma,curve=curve.gamma))
    
    return(re)
  }else
  {
    return(FMA.concurrent(Z,M,Y,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,lambda.m=lambda.m,lambda.y=lambda.y))
  }
}
