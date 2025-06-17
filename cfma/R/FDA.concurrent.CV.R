FDA.concurrent.CV <-
function(X,Y,intercept=TRUE,basis=NULL,Ld2.basis=NULL,basis.type=c("fourier"),nbasis=3,timeinv=c(0,1),timegrids=NULL,
                            lambda=10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1]),nfolds=5,verbose=TRUE)
{
  N<-dim(X)[1]             # # of subject
  ntp<-dim(X)[2]           # # of time points
  
  idx.tmp<-sample(1:N,N,replace=FALSE)
  cv.idx<-split(idx.tmp,sort(idx.tmp%%nfolds))
  
  mse.fit<-matrix(NA,N,length(lambda))
  
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
    
    for(ii in 1:length(lambda))
    {
      re<-FDA.concurrent(Xtmp,Ytmp,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,lambda=lambda[ii])
      
      Xts.design<-design.mat(Xts,intercept=intercept)
      yfit<-t(apply(Xts.design$W,1,function(x){return(apply(x*t(re$gamma.curve),1,sum))}))
      
      mse.fit[idx.ts,ii]<-apply((yfit-Yts)^2,1,int.func,timeinv=timeinv,timegrids=timegrids)
    }
    
    if(verbose)
    {
      print(paste0("Fold ",kk))
    }
  }
  
  lambda.idx<-which.min(apply(mse.fit,2,mean))[1]
  lambda.est<-lambda[lambda.idx]
  re.fit<-FDA.concurrent(X,Y,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,lambda=lambda.est)
  re.fit$lambda<-lambda.est
  re.fit$mse<-mse.fit
  
  return(re.fit)
}
