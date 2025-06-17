FDA.concurrent <-
function(X,Y,intercept=TRUE,basis=NULL,Ld2.basis=NULL,basis.type=c("fourier"),nbasis=3,timeinv=c(0,1),timegrids=NULL,lambda=0.01)
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
  if(is.null(basis))
  {
    if(basis.type[1]=="fourier")
    {
      basis<-fourier.basis(timeinv=timeinv,ntp=ntp,nbasis=nbasis)
      
      Ld2.basis<-Ld2.fourier(timeinv=timeinv,ntp=ntp,nbasis=nbasis)
    }
  }else
  {
    nbasis<-ncol(basis)
  }
  
  # lambda
  if(length(lambda)==1)
  {
    lambda<-rep(lambda,q)
  }else
    if(length(lambda)>q)
    {
      lambda<-lambda[1:q]
    }else
    {
      lambda<-rep(lambda[1],q)
    }
  
  K<-nbasis*q
  
  U<-matrix(0,K,K)
  Theta<-array(0,c(q,K,ntp))
  for(j in 1:q)
  {
    Utmp<-lambda[j]*apply(array(apply(Ld2.basis,1,function(x){return(x%*%t(x))}),c(nbasis,nbasis,ntp)),c(1,2),function(x){return(int.func(x,timeinv=timeinv,timegrids=timegrids))})
    U[((j-1)*nbasis+1):(j*nbasis),((j-1)*nbasis+1):(j*nbasis)]<-Utmp
    
    Theta[j,((j-1)*nbasis+1):(j*nbasis),]<-t(basis)
  }
  
  # W %*% Theta matrix
  mat1<-array(NA,c(K,K,ntp))
  mat2<-array(NA,c(K,1,ntp))
  for(s in 1:ntp)
  {
    dtmp<-matrix(W[,s,],ncol=q)%*%matrix(Theta[,,s],nrow=q)
    mat1[,,s]<-t(dtmp)%*%dtmp
    mat2[,,s]<-t(matrix(Theta[,,s],nrow=q))%*%t(matrix(W[,s,],ncol=q))%*%Y[,s]
  }
  V1<-apply(mat1,c(1,2),int.func,timeinv=timeinv,timegrids=timegrids)
  V2<-apply(mat2,c(1,2),int.func,timeinv=timeinv,timegrids=timegrids)
  g<-solve(V1+U)%*%V2
  
  # gamma.est<-matrix(apply(Theta,3,function(x){return(x%*%g)}),ncol=q)
  gamma.est<-matrix(NA,ntp,q)
  for(j in 1:q)
  {
    gamma.est[,j]<-basis%*%g[((j-1)*nbasis+1):(j*nbasis)]
  }
  if(intercept)
  {
    colnames(gamma.est)<-c("Intercept",paste0("X",1:(q-1))) 
  }else
  {
    colnames(gamma.est)<-paste0("X",1:q)
  }
  yfit<-t(apply(W,1,function(x){return(apply(x*gamma.est,1,sum))}))
  
  re<-list(coefficients=c(g),basis=basis,gamma.curve=t(gamma.est),fitted=yfit)
  
  return(re)
}
