FMA.historical.CV <-
function(Z,M,Y,delta.grid1=1,delta.grid2=1,delta.grid3=1,intercept=TRUE,basis1=NULL,Ld2.basis1=NULL,basis2=NULL,Ld2.basis2=NULL,basis.type=c("fourier"),
                             nbasis1=3,nbasis2=3,timeinv=c(0,1),timegrids=NULL,
                             lambda1=NULL,lambda2=NULL,nfolds=5)
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

  if(is.null(lambda1))
  {
    lambda1<-10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1])
  }
  if(is.null(lambda2))
  {
    lambda2<-10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1])
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
  fit.m<-FDA.historical.CV(Z,M,delta.grid=delta.grid1,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                           nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda1,lambda2=lambda2,nfolds=nfolds,verbose=FALSE)
  lambda1.m<-fit.m$lambda1
  lambda2.m<-fit.m$lambda2
  # Y model
  fit.y<-FDA.historical2.CV(X1=Z,X2=M,Y,delta.grid1=delta.grid2,delta.grid2=delta.grid3,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                            nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda1,lambda2=lambda2,nfolds=nfolds,verbose=FALSE)
  lambda1.y<-fit.y$lambda1
  lambda2.y<-fit.y$lambda2
  
  if(intercept)
  {
    # M model
    coef.inter.m<-fit.m$coefficients[1:nbasis1,1:nbasis2]
    curve.inter.m<-fit.m$gamma.curve[,,1]
    
    coef.alpha<-fit.m$coefficients[(nbasis1+1):(2*nbasis1),(nbasis2+1):(2*nbasis2)]
    curve.alpha<-fit.m$gamma.curve[,,2]
    
    # Y model
    coef.inter.y<-fit.y$coefficients[1:nbasis1,1:nbasis2]
    curve.inter.y<-fit.y$gamma.curve[,,1]
    
    coef.gamma<-fit.y$coefficients[(nbasis1+1):(2*nbasis1),(nbasis2+1):(2*nbasis2)]
    curve.gamma<-fit.y$gamma.curve[,,2]
    
    coef.beta<-fit.y$coefficients[(2*nbasis1+1):(3*nbasis1),(2*nbasis2+1):(3*nbasis2)]
    curve.beta<-fit.y$gamma.curve[,,3]
    
    # IE and DE
    curve.IE<-rep(NA,ntp)
    curve.DE<-rep(NA,ntp)
    alpha.int<-rep(NA,ntp)
    for(i in 1:ntp)
    {
      rtmp1<-max(i-delta.grid1,1)
      rtmp2<-max(i-delta.grid2,1)
      rtmp3<-max(i-delta.grid3,1)
      
      alpha.int[i]<-int.func(curve.alpha[rtmp1:i,i],timeinv=c(timegrids[rtmp1],timegrids[i]),timegrids=timegrids[rtmp1:i])
      
      curve.IE[i]<-int.func(alpha.int[rtmp3:i]*curve.beta[rtmp3:i,i],timeinv=c(timegrids[rtmp3],timegrids[i]),timegrids=timegrids[rtmp3:i])
      
      curve.DE[i]<-int.func(curve.gamma[rtmp2:i,i],timeinv=c(timegrids[rtmp2],timegrids[i]),timegrids=timegrids[rtmp2:i])
    }
    
    coef.m<-list(Intercept=coef.inter.m,alpha=coef.alpha)
    curve.m<-list(Intercept=curve.inter.m,alpha=curve.alpha)
    re.m<-list(coefficients=coef.m,curve=curve.m,fitted=fit.m$fitted,lambda1=lambda1.m,lambda2=lambda2.m)
    
    coef.y<-list(Intercept=coef.inter.y,gamma=coef.gamma,beta=coef.beta)
    curve.y<-list(Intercept=curve.inter.y,gamma=curve.gamma,beta=curve.beta)
    re.y<-list(coefficients=coef.y,curve=curve.y,fitted=fit.y$fitted,lambda1=lambda1.y,lambda2=lambda2.y)
    
    re.IE<-list(curve=curve.IE)
    re.DE<-list(curve=curve.DE)
    
    re<-list(basis1=basis1,basis2=basis2,M=re.m,Y=re.y,IE=re.IE,DE=re.DE)
  }else
  {
    # M model
    coef.alpha<-fit.m$coefficients[1:nbasis1,1:nbasis2]
    curve.alpha<-fit.m$gamma.curve[,,1]
    
    # Y model
    coef.gamma<-fit.y$coefficients[1:nbasis1,1:nbasis2]
    curve.gamma<-fit.y$gamma.curve[,,1]
    
    coef.beta<-fit.y$coefficients[(nbasis1+1):(2*nbasis1),(nbasis2+1):(2*nbasis2)]
    curve.beta<-fit.y$gamma.curve[,,2]
    
    # IE and DE
    curve.IE<-rep(NA,ntp)
    curve.DE<-rep(NA,ntp)
    alpha.int<-rep(NA,ntp)
    for(i in 1:ntp)
    {
      rtmp1<-max(i-delta.grid1,1)
      rtmp2<-max(i-delta.grid2,1)
      rtmp3<-max(i-delta.grid3,1)
      
      alpha.int[i]<-int.func(curve.alpha[rtmp1:i,i],timeinv=c(timegrids[rtmp1],timegrids[i]),timegrids=timegrids[rtmp1:i])
      
      curve.IE[i]<-int.func(alpha.int[rtmp3:i]*curve.beta[rtmp3:i,i],timeinv=c(timegrids[rtmp3],timegrids[i]),timegrids=timegrids[rtmp3:i])
      
      curve.DE[i]<-int.func(curve.gamma[rtmp2:i,i],timeinv=c(timegrids[rtmp2],timegrids[i]),timegrids=timegrids[rtmp2:i])
    }
    
    coef.m<-list(alpha=coef.alpha)
    curve.m<-list(alpha=curve.alpha)
    re.m<-list(coefficients=coef.m,curve=curve.m,fitted=fit.m$fitted,lambda1=lambda1.m,lambda2=lambda2.m)
    
    coef.y<-list(gamma=coef.gamma,beta=coef.beta)
    curve.y<-list(gamma=curve.gamma,beta=curve.beta)
    re.y<-list(coefficients=coef.y,curve=curve.y,fitted=fit.y$fitted,lambda1=lambda1.y,lambda2=lambda2.y)
    
    re.IE<-list(curve=curve.IE)
    re.DE<-list(curve=curve.DE)
    
    re<-list(basis1=basis1,basis2=basis2,M=re.m,Y=re.y,IE=re.IE,DE=re.DE)
  }
  
  return(re)
}
