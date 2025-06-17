designD.mat <-
function(X,intercept=TRUE,basis1=NULL,Ld2.basis1=NULL,basis2=NULL,Ld2.basis2=NULL,basis.type=c("fourier"),nbasis1=3,nbasis2=3,timeinv=c(0,1),timegrids=NULL,delta.grid=1)
{
  N<-dim(X)[1]             # # of subject
  ntp<-dim(X)[2]           # # of time points
  
  # design matrix
  X.design<-design.mat(X,intercept=intercept)
  q<-X.design$q
  W<-X.design$W
  
  # time grids
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
  
  # lambda
  if(length(lambda1)==1)
  {
    lambda1<-rep(lambda1,q)
  }else
    if(length(lambda1)>q)
    {
      lambda1<-lambda1[1:q]
    }else
    {
      lambda1<-rep(lambda1[1],q)
    }
  if(length(lambda2)==1)
  {
    lambda2<-rep(lambda2,q)
  }else
    if(length(lambda2)>q)
    {
      lambda2<-lambda2[1:q]
    }else
    {
      lambda2<-rep(lambda2[1],q)
    }
  
  
  K1<-nbasis1*q
  K2<-nbasis2*q
  K<-(nbasis1*nbasis2)*q
  ######################################################
  U<-matrix(0,K,K)
  V<-matrix(0,K,K)
  D<-array(0,c(N,K,ntp))
  for(j in 1:q)
  {
    U1tmp<-apply(array(apply(basis2,1,function(x){return(x%*%t(x))}),c(nbasis2,nbasis2,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
    U2tmp<-apply(array(apply(Ld2.basis1,1,function(x){return(x%*%t(x))}),c(nbasis1,nbasis1,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
    U[((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2)),((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2))]<-lambda1[j]*kronecker(U1tmp,U2tmp)
    
    V1tmp<-apply(array(apply(Ld2.basis2,1,function(x){return(x%*%t(x))}),c(nbasis2,nbasis2,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
    V2tmp<-apply(array(apply(basis1,1,function(x){return(x%*%t(x))}),c(nbasis1,nbasis1,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
    V[((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2)),((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2))]<-lambda2[j]*kronecker(V1tmp,V2tmp)
    
    
    Ws<-array(NA,c(N,nbasis1,ntp))
    Wstmp<-array(NA,c(N,nbasis1,ntp))
    for(i in 1:ntp)
    {
      Wstmp[,,i]<-matrix(W[,i,j],nrow=N)%*%matrix(basis1[i,],ncol=nbasis1)
      
      rtmp<-max(i-delta.grid,1)
      
      Ws[,,i]<-apply(array(Wstmp[,,rtmp:i],c(N,nbasis1,length(rtmp:i))),c(1,2),int.func,timeinv=c(timegrids[rtmp],timegrids[i]),timegrids=timegrids[rtmp:i])
      
      D[,((j-1)*(nbasis1*nbasis2)+1):(j*(nbasis1*nbasis2)),i]<-kronecker(t(basis2[i,]),Ws[,,i])
    }
  }
  
  return(list(D=D,U=U,V=V))
}
