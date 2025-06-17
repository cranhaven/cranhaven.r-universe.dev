FDA.historical2.CV <-
function(X1,X2,Y,delta.grid1=1,delta.grid2=1,intercept=TRUE,basis1=NULL,Ld2.basis1=NULL,basis2=NULL,Ld2.basis2=NULL,basis.type=c("fourier"),nbasis1=3,nbasis2=3,timeinv=c(0,1),timegrids=NULL,
                             lambda1=10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1]),lambda2=10^c(seq(-2,1,length.out=20),seq(1,3,length.out=11)[-1]),nfolds=5,verbose=TRUE)
{
  N<-dim(X1)[1]             # # of subject
  ntp<-dim(X1)[2]           # # of time points
  
  idx.tmp<-sample(1:N,N,replace=FALSE)
  cv.idx<-split(idx.tmp,sort(idx.tmp%%nfolds))
  
  mse.fit<-matrix(NA,N,min(length(lambda1),length(lambda2)))
  
  for(kk in 1:length(cv.idx))
  {
    idx.ts<-cv.idx[[kk]]
    idx.tr<-unlist(cv.idx[-kk])
    
    Ytmp<-Y[idx.tr,]
    Yts<-matrix(Y[idx.ts,],nrow=length(idx.ts))
    if(length(dim(X1))==2)
    {
      X1tmp<-X1[idx.tr,]
      X1ts<-matrix(X1[idx.ts,],nrow=length(idx.ts))
    }
    if(length(dim(X1))==3)
    {
      X1tmp<-X1[idx.tr,,]
      X1ts<-array(X1[idx.ts,,],c(length(idx.ts),dim(X1)[2],dim(X1)[3]))
    }
    if(length(dim(X2))==2)
    {
      X2tmp<-X2[idx.tr,]
      X2ts<-matrix(X2[idx.ts,],nrow=length(idx.ts))
    }
    if(length(dim(X2))==3)
    {
      X2tmp<-X2[idx.tr,,]
      X2ts<-array(X2[idx.ts,,],c(length(idx.ts),dim(X2)[2],dim(X2)[3]))
    }
    
    for(ii in 1:min(length(lambda1),length(lambda2)))
    {
      re<-FDA.historical2(X1tmp,X2tmp,Ytmp,delta.grid1=delta.grid1,delta.grid2=delta.grid2,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                          nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda1[ii],lambda2=lambda2[ii])
      
      X1tsD.mat<-designD.mat(X1ts,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,nbasis1=nbasis1,nbasis2=nbasis2,
                             timeinv=timeinv,timegrids=timegrids,delta.grid=delta.grid1)
      X2tsD.mat<-designD.mat(X2ts,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,nbasis1=nbasis1,nbasis2=nbasis2,
                             timeinv=timeinv,timegrids=timegrids,delta.grid=delta.grid2)
      D1<-X1tsD.mat$D
      D2<-X2tsD.mat$D
      D<-array(NA,c(dim(D1)[1],dim(D1)[2]+dim(D2)[2],dim(D1)[3]))
      D[,1:(dim(D1)[2]),]<-D1
      D[,(dim(D1)[2]+1):(dim(D1)[2]+dim(D2)[2]),]<-D2
      
      yfit<-apply(D,c(1,3),function(x){return(t(x)%*%c(re$coef.vec))})
      
      mse.fit[idx.ts,ii]<-apply((yfit-Yts)^2,1,int.func,timeinv=timeinv,timegrids=timegrids)
    }
    
    if(verbose)
    {
      print(paste0("Fold ",kk))
    }
  }
  
  lambda.idx<-which.min(apply(mse.fit,2,mean))[1]
  lambda.est<-lambda1[lambda.idx]
  re.fit<-FDA.historical2(X1,X2,Y,delta.grid1=delta.grid1,delta.grid2=delta.grid2,intercept=intercept,basis1=basis1,Ld2.basis1=Ld2.basis1,basis2=basis2,Ld2.basis2=Ld2.basis2,basis.type=basis.type,
                          nbasis1=nbasis1,nbasis2=nbasis2,timeinv=timeinv,timegrids=timegrids,lambda1=lambda.est,lambda2=lambda.est)
  re.fit$lambda1<-lambda.est
  re.fit$lambda2<-lambda.est
  re.fit$mse<-mse.fit
  
  return(re.fit)
}
