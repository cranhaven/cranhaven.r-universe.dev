FDA.historical.CV <-
function(X,Y,delta.grid=1,intercept=TRUE,basis1=NULL,Ld2.basis1=NULL,basis2=NULL,Ld2.basis2=NULL,basis.type=c("fourier"),nbasis1=3,nbasis2=3,timeinv=c(0,1),timegrids=NULL,
                            lambda1=10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1]),lambda2=10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1]),nfolds=5,verbose=TRUE)
{
  N<-dim(X)[1]             # # of subject
  ntp<-dim(X)[2]           # # of time points
  
  idx.tmp<-sample(1:N,N,replace=FALSE)
  cv.idx<-split(idx.tmp,sort(idx.tmp%%nfolds))
  
  mse.fit<-matrix(NA,N,min(length(lambda1),length(lambda2)))
  
  for(kk in 1:length(cv.idx))
  {
    idx.ts<-cv.idx[[kk]]
    idx.tr<-unlist(cv.idx[-kk])
    
    Ytmp<-Y[idx.tr,]
    Yts<-matrix(Y[idx.ts,],nrow=length(idx.ts))
    if(length(dim(X))==2)
    {
      Xtmp<-X[idx.tr,]
      Xts<-matrix(X[idx.ts,],nrow=length(idx.ts))
    }
    if(length(dim(X))==3)
    {
      Xtmp<-X[idx.tr,,]
      Xts<-array(X[idx.ts,,],c(length(idx.ts),dim(X)[2],dim(X)[3]))
    }
    
    for(ii in 1:min(length(lambda1),length(lambda2)))
    {
      re<-FDA.historical(Xtmp,Ytmp,delta.grid=delta.grid,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                         nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda1[ii],lambda2=lambda2[ii])
      
      XtsD.mat<-designD.mat(Xts,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,nbasis1=nbasis1,nbasis2=nbasis2,
                            timeinv=timeinv,timegrids=timegrids,delta.grid=delta.grid)
      yfit<-apply(XtsD.mat$D,c(1,3),function(x){return(t(x)%*%c(re$coef.vec))})
      
      mse.fit[idx.ts,ii]<-apply((yfit-Yts)^2,1,int.func,timeinv=timeinv,timegrids=timegrids)
    }
    
    if(verbose)
    {
      print(paste0("Fold ",kk))
    }
  }
  
  lambda.idx<-which.min(apply(mse.fit,2,mean))[1]
  lambda.est<-lambda1[lambda.idx]
  re.fit<-FDA.historical(X,Y,delta.grid=delta.grid,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                         nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda.est,lambda2=lambda.est)
  re.fit$lambda1<-lambda.est
  re.fit$lambda2<-lambda.est
  re.fit$mse<-mse.fit
  
  return(re.fit)
}
